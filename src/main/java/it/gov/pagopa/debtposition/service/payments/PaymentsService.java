package it.gov.pagopa.debtposition.service.payments;

import feign.FeignException;
import it.gov.pagopa.debtposition.client.NodeClient;
import it.gov.pagopa.debtposition.client.SendClient;
import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.checkposition.NodeCheckPositionModel;
import it.gov.pagopa.debtposition.model.checkposition.NodePosition;
import it.gov.pagopa.debtposition.model.checkposition.response.NodeCheckPositionResponse;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.send.response.NotificationPriceResponse;
import it.gov.pagopa.debtposition.repository.InstallmentRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Service
@Slf4j
public class PaymentsService {
    private final InstallmentRepository installmentRepository;
    private final PaymentPositionRepository paymentPositionRepository;
    private final NodeClient nodeClient;
    private final SendClient sendClient;

    @Autowired
    public PaymentsService(
            PaymentPositionRepository paymentPositionRepository,
            NodeClient nodeClient,
            SendClient sendClient,
            InstallmentRepository installmentRepository) {
        this.paymentPositionRepository = paymentPositionRepository;
        this.nodeClient = nodeClient;
        this.sendClient = sendClient;
        this.installmentRepository = installmentRepository;
    }

    @Value("${nav.aux.digit}")
    private String auxDigit;

    // TODO #naviuv: temporary regression management --> the nav variable can also be evaluated with
    // iuv. Remove the comment when only nav managment is enabled
    @Transactional
    public Installment getInstallmentByNav(
            @NotBlank String organizationFiscalCode, @NotBlank String nav) {

        Optional<Installment> installmentOpt =
                installmentRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
                        organizationFiscalCode, nav, organizationFiscalCode, nav);

        if (installmentOpt.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
        }

        Installment installment = installmentOpt.get();
        // Update PaymentPosition instance only in memory
        // PaymentPosition used when converting PaymentOption to POWithDebtor
        DebtPositionStatus.validityCheckAndUpdate(installment);
        DebtPositionStatus.expirationCheckAndUpdate(installment);
        DebtPositionStatus.checkAlreadyPaidInstallments(installment.getPaymentOption(), nav);

        // Synchronous update of notification fees
        if (Boolean.TRUE.equals(installment.getSendSync())) {
            boolean result = this.updateNotificationFeeSync(installment);
            if (result)
                log.info(
                        "Notification fee amount of Installment with NAV {} has been updated with"
                                + " notification-fee: {}.",
                        installment.getNav(),
                        installment.getNotificationFee());
            else
                log.error(
                        "[GPD-ERR-SEND-01] Error while updating notification fee amount for NAV {}.",
                        installment.getNav());
        }

        return installment;
    }

    @Transactional
    public Installment pay(
            @NotBlank String organizationFiscalCode,
            @NotBlank String nav,
            @NotNull @Valid PaymentOptionModel paymentOptionModel) {
        Optional<PaymentPosition> paymentPositionToPayOpt =
                paymentPositionRepository
                        .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionInstallmentIuvOrPaymentOptionOrganizationFiscalCodeAndPaymentOptionInstallmentNav(
                                organizationFiscalCode, nav, organizationFiscalCode, nav);

        if (paymentPositionToPayOpt.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
        }

        PaymentPosition paymentPositionToPay = paymentPositionToPayOpt.get();

        // Update PaymentPosition instance only in memory
        DebtPositionStatus.validityCheckAndUpdate(paymentPositionToPay);
        DebtPositionValidation.checkPaymentPositionPayability(paymentPositionToPay, nav);

        return this.executePaymentFlow(paymentPositionToPay, nav, paymentOptionModel);
    }

    @Transactional
    public Transfer report(
            @NotBlank String organizationFiscalCode, @NotBlank String iuv, @NotBlank String transferId) {

        Optional<PaymentPosition> ppToReport =
                paymentPositionRepository
                        .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionInstallmentIuvAndPaymentOptionInstallmentTransferTransferId(
                                organizationFiscalCode, iuv, transferId);

        if (ppToReport.isEmpty()) {
            throw new AppException(AppError.TRANSFER_NOT_FOUND, organizationFiscalCode, iuv, transferId);
        }

        DebtPositionValidation.checkPaymentPositionAccountability(ppToReport.get(), iuv, transferId);

        return this.updateTransferStatus(ppToReport.get(), iuv, transferId);
    }

    private boolean updateNotificationFeeSync(Installment installment) {
        try {
            // call SEND API to retrieve notification fee amount
            NotificationPriceResponse sendResponse =
                    sendClient.getNotificationFee(
                            installment.getOrganizationFiscalCode(), installment.getNav());
            int notificationFeeAmount = sendResponse.getTotalPrice();
            // call internal method updateAmountsWithNotificationFee
            updateAmountsWithNotificationFee(
                    installment, installment.getOrganizationFiscalCode(), notificationFeeAmount);
            // track the PO last update
            installment.setLastUpdatedDate(LocalDateTime.now(ZoneOffset.UTC));
            installment.setLastUpdatedDateNotificationFee(LocalDateTime.now(ZoneOffset.UTC));

            installmentRepository.saveAndFlush(installment);
            return true;
        } catch (Exception e) {
            log.error(
                    "[GPD-ERR-SEND-00] Exception while calling getNotificationFee for NAV {}, class = {},"
                            + " message = {}.",
                    installment.getNav(),
                    e.getClass(),
                    e.getMessage());
            return false;
        }
    }

    @Transactional
    public Installment updateNotificationFee(@NotBlank String organizationFiscalCode, @NotBlank String nav, Long notificationFeeAmount) {

        // Check if exists a payment option with the passed IUV related to the organization
        // TODO #naviuv: temporary regression management: search by nav or iuv
        Optional<Installment> installmentOpt =
                installmentRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
                        organizationFiscalCode, nav, organizationFiscalCode, nav);
        if (installmentOpt.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
        }
        // Check if the retrieved payment option was not already paid and/or reported
        Installment installment = installmentOpt.get();
        if (InstallmentStatus.getInstallmentPaidStatus().contains(installment.getStatus())) {
            throw new AppException(
                    AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_NOT_UPDATABLE,
                    organizationFiscalCode,
                    nav);
        }

        // Executing the amount updating with the inserted notification fee
        updateAmountsWithNotificationFee(installment, organizationFiscalCode, notificationFeeAmount);

        // Executes a call to the node's checkPosition API to see if there is a payment in progress
        try {
            // TODO #naviuv: temporary regression management: search by nav or iuv --> possible double
            // call to the node
            // 1. first call attempt is with the nav variable valued as iuv (auxDigit added)
            NodePosition position =
                    NodePosition.builder()
                            .fiscalCode(organizationFiscalCode)
                            .noticeNumber(auxDigit + nav)
                            .build();
            NodeCheckPositionResponse chkPositionRes =
                    nodeClient.getCheckPosition(
                            NodeCheckPositionModel.builder()
                                    .positionslist(Collections.singletonList(position))
                                    .build());
            installment.setPaymentInProgress(
                    "OK".equalsIgnoreCase(chkPositionRes.getOutcome()) ? Boolean.FALSE : Boolean.TRUE);
        } catch (FeignException.BadRequest e) {
            // 2. if the first call fails with a bad request error --> try with a nav call
            NodePosition position =
                    NodePosition.builder().fiscalCode(organizationFiscalCode).noticeNumber(nav).build();
            try {
                NodeCheckPositionResponse chkPositionRes =
                        nodeClient.getCheckPosition(
                                NodeCheckPositionModel.builder()
                                        .positionslist(Collections.singletonList(position))
                                        .build());
                installment.setPaymentInProgress(
                        "OK".equalsIgnoreCase(chkPositionRes.getOutcome()) ? Boolean.FALSE : Boolean.TRUE);
            } catch (Exception ex) {
                log.error(
                        "Error checking the position on the node for PO with fiscalCode "
                                + organizationFiscalCode
                                + " and noticeNumber "
                                + "("
                                + auxDigit
                                + ")"
                                + nav,
                        ex);
                // By business rules it is expected to treat the error as if the node had responded KO
                installment.setPaymentInProgress(Boolean.TRUE);
            }
        } catch (Exception e) {
            log.error(
                    "Error checking the position on the node for PO with fiscalCode "
                            + organizationFiscalCode
                            + " and noticeNumber "
                            + "("
                            + auxDigit
                            + ")"
                            + nav,
                    e);
            // By business rules it is expected to treat the error as if the node had responded KO
            installment.setPaymentInProgress(Boolean.TRUE);
        }

        // Updated to track the PO update
        installment.setLastUpdatedDate(LocalDateTime.now(ZoneOffset.UTC));
        installment.setLastUpdatedDateNotificationFee(LocalDateTime.now(ZoneOffset.UTC));

        installmentRepository.saveAndFlush(installment);
        return installment;
    }

    public static void updateAmountsWithNotificationFee(
            Installment installment, String organizationFiscalCode, long notificationFeeAmount) {
        // Get the first valid transfer to add the fee
        Transfer validTransfer = findPrimaryTransfer(installment, organizationFiscalCode);

    /*
    Retrieving the old notification fee. It MUST BE SUBTRACTED from the various amount in order due to the fact that
    these values were updated in a previous step with another value and adding the new value directly can cause miscalculations.
     */
        long oldNotificationFee = Optional.of(installment.getNotificationFee()).orElse(0L);

        // Setting the new value of the notification fee, updating the amount of the payment option and
        // the last updated date fee
        installment.setNotificationFee(notificationFeeAmount);
        installment.setAmount(installment.getAmount() - oldNotificationFee);
        installment.setAmount(installment.getAmount() + notificationFeeAmount);

        // Subtracting the old value and adding the new one
        validTransfer.setAmount(validTransfer.getAmount() - oldNotificationFee);
        validTransfer.setAmount(validTransfer.getAmount() + notificationFeeAmount);
    }

    /**
     * find the primary transfer of the installment
     *
     * @param installment            the entity of the installment
     * @param organizationFiscalCode EC
     * @return the transfer of the primary Creditor Institution
     */
    public static Transfer findPrimaryTransfer(
            Installment installment, String organizationFiscalCode) {
        List<Transfer> transfers = installment.getTransfer();
        return transfers.stream()
                .sorted(Comparator.comparing(Transfer::getTransferId))
                .filter(
                        transfer ->
                                transfer.getOrganizationFiscalCode() == null
                                        || organizationFiscalCode.equals(transfer.getOrganizationFiscalCode()))
                .findFirst()
                .orElseThrow(
                        () ->
                                new AppException(
                                        AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_TRANSFER_NOT_FOUND,
                                        installment.getIuv(),
                                        organizationFiscalCode));
    }

    public List<OrganizationModelQueryBean> getOrganizationsToAdd(@NotNull LocalDate since) {
        return paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
    }

    public List<OrganizationModelQueryBean> getOrganizationsToDelete(@NotNull LocalDate since) {
        paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
        return Collections.emptyList();
    }

    private Installment executePaymentFlow(
            PaymentPosition pp, String nav, PaymentOptionModel paymentOptionModel) {

        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        PaymentOption paidPO = null;
        Installment paidInst = null;

        long numberOfPartialPayment =
                pp.getPaymentOption().stream()
                        .filter(po -> OptionType.OPZIONE_RATEALE.equals(po.getOptionType()))
                        .map(PaymentOption::getInstallment)
                        .mapToLong(List::size)
                        .sum();
        // verifico se ci sono pagamenti parziali in stato PAID
        long countPaidPartialPayment = pp.getPaymentOption().stream()
                .filter(po -> OptionType.OPZIONE_RATEALE.equals(po.getOptionType()))
                .map(PaymentOption::getInstallment)
                .flatMap(List::stream)
                .filter(inst -> InstallmentStatus.PAID.equals(inst.getStatus()))
                .count();

        for (PaymentOption po : pp.getPaymentOption()) {
            for (Installment inst : po.getInstallment()) {
                // aggiorno le proprietà per l'installment oggetti dell'attuale pagamento
                // TODO #naviuv: temporary regression management --> remove "|| po.getIuv().equals(nav)" when
                // only nav managment is enabled
                if (inst.getNav().equals(nav) || inst.getIuv().equals(nav)) {
                    inst.setLastUpdatedDate(currentDate);
                    inst.setPaymentDate(paymentOptionModel.getPaymentDate());
                    inst.setPaymentMethod(paymentOptionModel.getPaymentMethod());
                    inst.setPspCode(paymentOptionModel.getPspCode());
                    inst.setPspTaxCode(paymentOptionModel.getPspTaxCode());
                    inst.setPspCompany(paymentOptionModel.getPspCompany());
                    inst.setReceiptId(paymentOptionModel.getIdReceipt());
                    inst.setFee(Long.parseLong(paymentOptionModel.getFee()));
                    inst.setStatus(InstallmentStatus.PAID);
                    // se la payment option è di tipo partial incremento il contatore
                    if (OptionType.OPZIONE_RATEALE.equals(po.getOptionType())) {
                        countPaidPartialPayment++;
                    }

                    paidInst = inst;
                    paidPO = po;
                }
            }
        }

        // aggiorno lo stato della payment position
        // PIDM-42 if paying the full amount when there is already a paid partial payment
        // then update the payment position status to PAID
        if (countPaidPartialPayment > 0
                && countPaidPartialPayment < numberOfPartialPayment
                && OptionType.OPZIONE_RATEALE.equals(Objects.requireNonNull(paidPO).getOptionType())) {
            pp.setStatus(DebtPositionStatus.PARTIALLY_PAID);
        } else {
            pp.setStatus(DebtPositionStatus.PAID);
            pp.setPaymentDate(paymentOptionModel.getPaymentDate());
        }

        pp.setLastUpdatedDate(currentDate);

        // salvo l'aggiornamento del pagamento
        paymentPositionRepository.saveAndFlush(pp);
        return paidInst;
    }

    private Transfer updateTransferStatus(PaymentPosition pp, String iuv, String transferId) {
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        long numberPOTransfers = 0;
        long countReportedTransfer = 0;
        Transfer reportedTransfer = null;

        Optional<Installment> installmentToReport = pp.getPaymentOption().stream()
                .map(PaymentOption::getInstallment)
                .flatMap(List::stream)
                .filter(inst -> inst.getIuv().equals(iuv))
                .findFirst();

        if (installmentToReport.isPresent()) {
            Installment inst = installmentToReport.get();

            // numero totale dei transfer per la PO
            numberPOTransfers = inst.getTransfer().size();
            // numero dei transfer della PO in stato T_REPORTED
            countReportedTransfer =
                    inst.getTransfer().stream()
                            .filter(t -> t.getStatus().equals(TransferStatus.T_REPORTED))
                            .count();
            // recupero il transfer oggetto di rendicontazione
            Optional<Transfer> transferToReport =
                    inst.getTransfer().stream().filter(t -> t.getTransferId().equals(transferId)).findFirst();

            if (transferToReport.isEmpty()) {
                String error = String.format("Obtained unexpected empty transfer - [organizationFiscalCode= %s; iupd= %s; iuv= %s; idTransfer= %s]", pp.getOrganizationFiscalCode(), pp.getIupd(), iuv, transferId);
                throw new AppException(AppError.TRANSFER_REPORTING_FAILED, error);
            }

            reportedTransfer = transferToReport.get();

            reportedTransfer.setStatus(TransferStatus.T_REPORTED);
            reportedTransfer.setLastUpdatedDate(currentDate);
            countReportedTransfer++;
            // update installment status
            if (countReportedTransfer < numberPOTransfers) {
                inst.setStatus(InstallmentStatus.PARTIALLY_REPORTED);
            } else {
                inst.setStatus(InstallmentStatus.REPORTED);
            }
            inst.setLastUpdatedDate(currentDate);
        }

        this.setPaymentPositionStatus(pp);
        pp.setLastUpdatedDate(currentDate);
        // salvo l'aggiornamento della rendicontazione
        paymentPositionRepository.saveAndFlush(pp);

        return reportedTransfer;
    }

    private void setPaymentPositionStatus(PaymentPosition pp) {
        List<Installment> installmentList = pp.getPaymentOption().parallelStream()
                .map(PaymentOption::getInstallment)
                .flatMap(List::parallelStream).toList();
        // numero totale degli installments in opzione di pagamento in unica rata in stato REPORTED
        long numberInstallmentReportedNoPartial =
                installmentList.parallelStream()
                        .filter(
                                inst ->
                                        (inst.getStatus().equals(InstallmentStatus.REPORTED)
                                                && OptionType.OPZIONE_UNICA.equals(inst.getPaymentOption().getOptionType())))
                        .count();
        // numero totale degli installments rateizzati
        long totalNumberPartialInstallment =
                installmentList.parallelStream()
                        .filter(inst -> OptionType.OPZIONE_RATEALE.equals(inst.getPaymentOption().getOptionType()))
                        .count();
        // numero degli installments rateizzati in stato REPORTED
        long numberPOReportedPartial =
                installmentList.parallelStream()
                        .filter(
                                inst ->
                                        (inst.getStatus().equals(InstallmentStatus.REPORTED)
                                                && OptionType.OPZIONE_RATEALE.equals(inst.getPaymentOption().getOptionType())))
                        .count();

        if (numberInstallmentReportedNoPartial > 0
                || (totalNumberPartialInstallment > 0 && totalNumberPartialInstallment == numberPOReportedPartial)) {
            pp.setStatus(DebtPositionStatus.REPORTED);
        }
    }
}
