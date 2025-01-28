package it.gov.pagopa.debtposition.service.payments;

import feign.FeignException;
import it.gov.pagopa.debtposition.client.NodeClient;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.checkposition.NodeCheckPositionModel;
import it.gov.pagopa.debtposition.model.checkposition.NodePosition;
import it.gov.pagopa.debtposition.model.checkposition.response.NodeCheckPositionResponse;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.TransferRepository;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;


@Service
@Slf4j
public class PaymentsService {

    private final PaymentOptionRepository paymentOptionRepository;
    private final PaymentPositionRepository paymentPositionRepository;
    private final TransferRepository transferRepository;
    private final NodeClient nodeClient;

    @Autowired
    public PaymentsService(PaymentPositionRepository paymentPositionRepository, PaymentOptionRepository paymentOptionRepository, TransferRepository transferRepository, NodeClient nodeClient) {
        this.paymentPositionRepository = paymentPositionRepository;
        this.paymentOptionRepository = paymentOptionRepository;
        this.transferRepository = transferRepository;
        this.nodeClient = nodeClient;
    }


    @Value("${nav.aux.digit}")
    private String auxDigit;

    //TODO #naviuv: temporary regression management --> the nav variable can also be evaluated with iuv. Remove the comment when only nav managment is enabled
    public PaymentOption getPaymentOptionByNAV(@NotBlank String organizationFiscalCode,
                                               @NotBlank String nav) {

        Optional<PaymentOption> po = paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(organizationFiscalCode, nav, organizationFiscalCode, nav);

        if (po.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
        }

        PaymentOption paymentOption = po.get();
        // Update PaymentPosition instance only in memory
        // PaymentPosition used when converting PaymentOption to POWithDebtor
        DebtPositionStatus.validityCheckAndUpdate(paymentOption);
        DebtPositionStatus.expirationCheckAndUpdate(paymentOption);

        return paymentOption;
    }


    @Transactional
    public PaymentOption pay(@NotBlank String organizationFiscalCode,
                             @NotBlank String nav, @NotNull @Valid PaymentOptionModel paymentOptionModel) {
        Optional<PaymentPosition> ppToPay = paymentPositionRepository.
                findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvOrPaymentOptionOrganizationFiscalCodeAndPaymentOptionNav(organizationFiscalCode, nav, organizationFiscalCode, nav);

        if (ppToPay.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
        }

        // Update PaymentPosition instance only in memory
        DebtPositionStatus.validityCheckAndUpdate(ppToPay.get());
        DebtPositionValidation.checkPaymentPositionPayability(ppToPay.get(), nav);

        return this.updatePaymentStatus(ppToPay.get(), nav, paymentOptionModel);
    }


    @Transactional
    public Transfer report(@NotBlank String organizationFiscalCode,
                           @NotBlank String iuv, @NotBlank String transferId) {


        Optional<PaymentPosition> ppToReport =
                paymentPositionRepository.findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(organizationFiscalCode, iuv, transferId);

        if (ppToReport.isEmpty()) {
            throw new AppException(AppError.TRANSFER_NOT_FOUND, organizationFiscalCode, iuv, transferId);
        }

        DebtPositionValidation.checkPaymentPositionAccountability(ppToReport.get(), iuv, transferId);

        return this.updateTransferStatus(ppToReport.get(), iuv, transferId);
    }

    @Transactional
    public PaymentOption updateNotificationFee(@NotBlank String organizationFiscalCode, @NotBlank String nav, Long notificationFeeAmount) {

        // Check if exists a payment option with the passed IUV related to the organization
        // TODO #naviuv: temporary regression management: search by nav or iuv
        Optional<PaymentOption> paymentOptionOpt = paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(organizationFiscalCode, nav, organizationFiscalCode, nav);
        if (paymentOptionOpt.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
        }
        // Check if the retrieved payment option was not already paid and/or reported
        PaymentOption paymentOption = paymentOptionOpt.get();
        if (!PaymentOptionStatus.PO_UNPAID.equals(paymentOption.getStatus())) {
            throw new AppException(AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_NOT_UPDATABLE, organizationFiscalCode, nav);
        }

        // Executing the amount updating with the inserted notification fee
        updateAmountsWithNotificationFee(paymentOption, organizationFiscalCode, notificationFeeAmount);

        // Executes a call to the node's checkPosition API to see if there is a payment in progress
        try {
            // TODO #naviuv: temporary regression management: search by nav or iuv --> possible double call to the node
            // 1. first call attempt is with the nav variable valued as iuv (auxDigit added)
            NodePosition position = NodePosition.builder().fiscalCode(organizationFiscalCode).noticeNumber(auxDigit + nav).build();
            NodeCheckPositionResponse chkPositionRes =
                    nodeClient.getCheckPosition(NodeCheckPositionModel.builder().positionslist(Collections.singletonList(position)).build());
            paymentOption.setPaymentInProgress("OK".equalsIgnoreCase(chkPositionRes.getOutcome()) ? Boolean.FALSE : Boolean.TRUE);
        } catch (FeignException.BadRequest e) {
            // 2. if the first call fails with a bad request error --> try with a nav call
            NodePosition position = NodePosition.builder().fiscalCode(organizationFiscalCode).noticeNumber(nav).build();
            try {
                NodeCheckPositionResponse chkPositionRes =
                        nodeClient.getCheckPosition(NodeCheckPositionModel.builder().positionslist(Collections.singletonList(position)).build());
                paymentOption.setPaymentInProgress("OK".equalsIgnoreCase(chkPositionRes.getOutcome()) ? Boolean.FALSE : Boolean.TRUE);
            } catch (Exception ex) {
                log.error("Error checking the position on the node for PO with fiscalCode " + organizationFiscalCode + " and noticeNumber " + "(" + auxDigit + ")" + nav, ex);
                // By business rules it is expected to treat the error as if the node had responded KO
                paymentOption.setPaymentInProgress(Boolean.TRUE);
            }
        } catch (Exception e) {
            log.error("Error checking the position on the node for PO with fiscalCode " + organizationFiscalCode + " and noticeNumber " + "(" + auxDigit + ")" + nav, e);
            // By business rules it is expected to treat the error as if the node had responded KO
            paymentOption.setPaymentInProgress(Boolean.TRUE);
        }

        // Updated to track the PO update
        paymentOption.setLastUpdatedDate(LocalDateTime.now(ZoneOffset.UTC));
        paymentOption.setLastUpdatedDateNotificationFee(LocalDateTime.now(ZoneOffset.UTC));

        paymentOptionRepository.saveAndFlush(paymentOption);
        return paymentOption;
    }

    public static PaymentOption updateAmountsWithNotificationFee(PaymentOption paymentOption, String organizationFiscalCode, long notificationFeeAmount) {
        // Get the first valid transfer to add the fee
        List<Transfer> transfers = paymentOption.getTransfer();
        Transfer validTransfer = transfers.stream()
                .sorted(Comparator.comparing(Transfer::getIdTransfer))
                .filter(transfer -> transfer.getOrganizationFiscalCode() == null || organizationFiscalCode.equals(transfer.getOrganizationFiscalCode()))
                .findFirst()
                .orElseThrow(() -> new AppException(AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_TRANSFER_NOT_FOUND, paymentOption.getIuv(), organizationFiscalCode));

        /*
        Retrieving the old notification fee. It MUST BE SUBTRACTED from the various amount in order due to the fact that
        these values were updated in a previous step with another value and adding the new value directly can cause miscalculations.
         */
        long oldNotificationFee = Optional.of(paymentOption.getNotificationFee()).orElse(0L);

        // Setting the new value of the notification fee, updating the amount of the payment option and the last updated date fee
        paymentOption.setNotificationFee(notificationFeeAmount);
        paymentOption.setAmount(paymentOption.getAmount() - oldNotificationFee);
        paymentOption.setAmount(paymentOption.getAmount() + notificationFeeAmount);

        // Subtracting the old value and adding the new one
        validTransfer.setAmount(validTransfer.getAmount() - oldNotificationFee);
        validTransfer.setAmount(validTransfer.getAmount() + notificationFeeAmount);

        return paymentOption;
    }

    public List<OrganizationModelQueryBean> getOrganizationsToAdd(@NotNull LocalDate since) {
        return paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
    }

    public List<OrganizationModelQueryBean> getOrganizationsToDelete(@NotNull LocalDate since) {
        paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
        return Collections.emptyList();
    }

    private PaymentOption updatePaymentStatus(PaymentPosition pp, String nav, PaymentOptionModel paymentOptionModel) {

        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        PaymentOption poToPay = null;

        long numberOfPartialPayment = pp.getPaymentOption().stream().filter(po -> Boolean.TRUE.equals(po.getIsPartialPayment())).count();
        int countPaidPartialPayment = 0;

        for (PaymentOption po : pp.getPaymentOption()) {

            // verifico se ci sono pagamenti parziali in stato PO_PAID
            if (Boolean.TRUE.equals(po.getIsPartialPayment()) && po.getStatus().equals(PaymentOptionStatus.PO_PAID)) {
                countPaidPartialPayment++;
            }

            // aggiorno le proprietà per la payment option oggetto dell'attuale pagamento
            // TODO #naviuv: temporary regression management --> remove "|| po.getIuv().equals(nav)" when only nav managment is enabled
            if (po.getNav().equals(nav) || po.getIuv().equals(nav)) {
                po.setLastUpdatedDate(currentDate);
                po.setPaymentDate(paymentOptionModel.getPaymentDate());
                po.setPaymentMethod(paymentOptionModel.getPaymentMethod());
                po.setPspCompany(paymentOptionModel.getPspCompany());
                po.setIdReceipt(paymentOptionModel.getIdReceipt());
                po.setFee(Long.parseLong(paymentOptionModel.getFee()));
                po.setStatus(PaymentOptionStatus.PO_PAID);
                // se la payment option è di tipo partial incremento il contatore
                if (Boolean.TRUE.equals(po.getIsPartialPayment())) {
                    countPaidPartialPayment++;
                }
                poToPay = po;
            }

        }

        // aggiorno lo stato della payment position
        if (countPaidPartialPayment > 0 && countPaidPartialPayment < numberOfPartialPayment) {
            pp.setStatus(DebtPositionStatus.PARTIALLY_PAID);
        } else {
            pp.setStatus(DebtPositionStatus.PAID);
            pp.setPaymentDate(paymentOptionModel.getPaymentDate());
        }

        pp.setLastUpdatedDate(currentDate);

        // salvo l'aggiornamento del pagamento
        paymentPositionRepository.saveAndFlush(pp);
        return poToPay;
    }

    private Transfer updateTransferStatus(PaymentPosition pp, String iuv, String transferId) {

        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        long numberPOTransfers = 0;
        long countReportedTransfer = 0;
        Transfer reportedTransfer = null;

        for (PaymentOption po : pp.getPaymentOption()) {

            if (po.getIuv().equals(iuv)) {
                //numero totale dei transfer per la PO
                numberPOTransfers = po.getTransfer().stream().count();
                //numero dei transfer della PO in stato T_REPORTED
                countReportedTransfer = po.getTransfer().stream().filter(t -> t.getStatus().equals(TransferStatus.T_REPORTED)).count();
                //recupero il transfer oggetto di rendicontazione
                Optional<Transfer> transferToReport = po.getTransfer().stream().filter(t -> t.getIdTransfer().equals(transferId)).findFirst();

                if (transferToReport.isEmpty()) {
                    log.error("Obtained unexpected empty transfer - "
                            + "[organizationFiscalCode= " + pp.getOrganizationFiscalCode() + "; "
                            + "iupd= " + pp.getIupd() + "; "
                            + "iuv= " + iuv + "; "
                            + "idTransfer= " + transferId
                            + "]");
                    throw new AppException(AppError.TRANSFER_REPORTING_FAILED, pp.getOrganizationFiscalCode(), iuv, transferId);
                }

                transferToReport.get().setStatus(TransferStatus.T_REPORTED);
                transferToReport.get().setLastUpdatedDate(currentDate);
                countReportedTransfer++;
                // aggiorno lo stato della PO
                if (countReportedTransfer < numberPOTransfers) {
                    po.setStatus(PaymentOptionStatus.PO_PARTIALLY_REPORTED);
                } else {
                    po.setStatus(PaymentOptionStatus.PO_REPORTED);
                }
                po.setLastUpdatedDate(currentDate);

                reportedTransfer = transferToReport.get();
            }

        }

        this.setPaymentPositionStatus(pp);
        pp.setLastUpdatedDate(currentDate);
        // salvo l'aggiornamento della rendicontazione
        paymentPositionRepository.saveAndFlush(pp);

        return reportedTransfer;
    }

    private void setPaymentPositionStatus(PaymentPosition pp) {
        //numero totale delle PO con pagamento in unica rata in stato PO_REPORTED
        long numberPOReportedNoPartial = pp.getPaymentOption().stream().filter(
                po -> (po.getStatus().equals(PaymentOptionStatus.PO_REPORTED) && Boolean.FALSE.equals(po.getIsPartialPayment()))).count();
        //numero totale delle PO rateizzate
        long totalNumberPartialPO = pp.getPaymentOption().stream().filter(po -> Boolean.TRUE.equals(po.getIsPartialPayment())).count();
        //numero delle PO rateizzate in stato PO_REPORTED
        long numberPOReportedPartial = pp.getPaymentOption().stream().filter(po -> (po.getStatus().equals(PaymentOptionStatus.PO_REPORTED) && Boolean.TRUE.equals(po.getIsPartialPayment()))).count();

        if (numberPOReportedNoPartial > 0 || (totalNumberPartialPO > 0 && totalNumberPartialPO == numberPOReportedPartial)) {
            pp.setStatus(DebtPositionStatus.REPORTED);
        }

    }

    // Update all Organization's IBANs on Transfer of payable PaymentPosition
    @Transactional
    public int updateTransferIbanMassive(String organizationFiscalCode, String oldIban, String newIban) {
        int numberOfUpdates = 0;
        // Retrieve all payment_position with organization_fiscal_code AND in status (DRAFT or PUBLISHED or VALID or PARTIALLY_PAID)
        List<PaymentPosition> ppToUpdate = paymentPositionRepository.findByOrganizationFiscalCodeAndStatusIn(organizationFiscalCode, List.of(DebtPositionStatus.DRAFT, DebtPositionStatus.PUBLISHED, DebtPositionStatus.VALID, DebtPositionStatus.PARTIALLY_PAID));
        if (ppToUpdate.isEmpty()) {
            throw new AppException(AppError.DEBT_POSITION_IN_UPDATABLE_STATE_NOT_FOUND, organizationFiscalCode);
        }
        // Retrieve all payment_option with payment_position_id AND in status PO_UNPAID
        List<PaymentOption> poToUpdate = paymentOptionRepository.findByPaymentPositionInAndStatusIn(ppToUpdate, List.of(PaymentOptionStatus.PO_UNPAID));

        // Update all Transfers that have the specified payment_option_id and oldIban as IBAN
        numberOfUpdates += transferRepository.updateTransferIban(poToUpdate, oldIban, newIban, LocalDateTime.now(ZoneOffset.UTC));

        return numberOfUpdates;
    }

}
