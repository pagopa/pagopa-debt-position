package it.gov.pagopa.debtposition.service.payments;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;


@Service
@Slf4j
public class PaymentsService {


    @Autowired
    private PaymentOptionRepository paymentOptionRepository;

    @Autowired
    private PaymentPositionRepository paymentPositionRepository;

    public PaymentOption getPaymentOptionByIUV(@NotBlank String organizationFiscalCode,
                                               @NotBlank String iuv) {

        Optional<PaymentOption> po = paymentOptionRepository.findByOrganizationFiscalCodeAndIuv(organizationFiscalCode, iuv);

        if (po.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, iuv);
        }

        return po.get();
    }


    @Transactional
    public PaymentOption pay(@NotBlank String organizationFiscalCode,
                             @NotBlank String iuv, @NotNull @Valid PaymentOptionModel paymentOptionModel) {


        Optional<PaymentPosition> ppToPay = paymentPositionRepository.findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuv(organizationFiscalCode, iuv);

        if (ppToPay.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, iuv);
        }

        DebtPositionValidation.checkPaymentPositionPayability(ppToPay.get(), iuv);

        return this.updatePaymentStatus(ppToPay.get(), iuv, paymentOptionModel);
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
    public PaymentOption updateNotificationFee(@NotBlank String organizationFiscalCode, @NotBlank String iuv, Long notificationFeeAmount) {

        // Check if exists a payment option with the passed IUV related to the organization
        Optional<PaymentOption> paymentOptionOpt = paymentOptionRepository.findByOrganizationFiscalCodeAndIuv(organizationFiscalCode, iuv);
        if (paymentOptionOpt.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, iuv);
        }
        // Check if the retrieved payment option was not already paid and/or reported
        PaymentOption paymentOption = paymentOptionOpt.get();
        if (!PaymentOptionStatus.PO_UNPAID.equals(paymentOption.getStatus())) {
            throw new AppException(AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_NOT_UPDATABLE, organizationFiscalCode, iuv);
        }
        //
        Collection<Transfer> transfers = paymentOption.getTransfer();
        Transfer validTransfer = transfers.stream()
                .filter(transfer -> organizationFiscalCode.equals(transfer.getOrganizationFiscalCode()))
                .findFirst()
                .orElseThrow(() -> new AppException(AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_TRANSFER_NOT_FOUND, paymentOption.getIuv(), organizationFiscalCode));

        /*
        Retrieving the old notification fee. It MUST BE SUBTRACTED from the various amount in order due to the fact that
        these values were updated in a previous step with another value and adding the new value directly can cause miscalculations.
         */
        long oldNotificationFee = Optional.of(paymentOption.getNotificationFee()).orElse(0L);

        // Setting the new value of the notification fee and updating the amount of the payment option
        paymentOption.setNotificationFee(notificationFeeAmount);
        paymentOption.setAmount(paymentOption.getAmount() - oldNotificationFee);
        paymentOption.setAmount(paymentOption.getAmount() + notificationFeeAmount);

        // Subtracting the old value and adding the new one
        validTransfer.setAmount(validTransfer.getAmount() - oldNotificationFee);
        validTransfer.setAmount(validTransfer.getAmount() + notificationFeeAmount);

        paymentOptionRepository.saveAndFlush(paymentOption);
        return paymentOption;
    }

    public List<OrganizationModelQueryBean> getOrganizationsToAdd(@NotNull LocalDate since) {
        return paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
    }

    public List<OrganizationModelQueryBean> getOrganizationsToDelete(@NotNull LocalDate since) {
        paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
        return Collections.emptyList();
    }

    private PaymentOption updatePaymentStatus(PaymentPosition pp, String iuv, PaymentOptionModel paymentOptionModel) {

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
            if (po.getIuv().equals(iuv)) {
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

        if (numberPOReportedNoPartial > 0 || totalNumberPartialPO == numberPOReportedPartial) {
            pp.setStatus(DebtPositionStatus.REPORTED);
        }

    }


}
