package it.gov.pagopa.debtposition.validation;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.exception.ValidationException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import lombok.extern.slf4j.Slf4j;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.List;

@Slf4j
public class DebtPositionValidation {

    private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; iupd= %s; iuv= %s";
    private static final String DUE_DATE_VALIDATION_ERROR = "Dates congruence error: due_date must be >= validity_date (if provided) or due_date must be >= current_date [due_date=%s; validity_date=%s; current_date=%s]";
    private static final String RETENTION_DATE_VALIDATION_ERROR = "Dates congruence error: retention_date must be >= due_date [retention_date=%s; due_date=%s] ";
    private static final String VALIDITY_DATE_VALIDATION_ERROR = "Dates congruence error: validity_date must be >= current_date [validity_date=%s; current_date=%s] ";
    private static final String AMOUNTS_VALIDATION_ERROR = "Amounts congruence error: payment option amount must coincide with the total of the transfers amount [payment_option_amount(in cent)=%s; total_tranfers_amount(in cent)=%s]";
    private static final String NUM_TRANSFERS_VALIDATION_ERROR = "Number of transfers congruence error: Each payment option must have a maximum of %s transactions [transactions found=%s]";
    private static final String TRANSFER_ID_VALIDATION_ERROR = "Transfer ID congruence error: The transaction id not have a value between those expected [transaction id=%s; expected values=%s]";

    private DebtPositionValidation() {
        super();
    }

    public static void checkPaymentPositionInputDataAccurancy(PaymentPosition pp) {
        checkPaymentPositionContentCongruency(pp);
    }

    public static void checkPaymentPositionPayability(PaymentPosition ppToPay, String iuv) {
        // Verifico se la posizione debitoria è in uno stato idoneo al pagamento
        if (DebtPositionStatus.getPaymentPosNotPayableStatus().contains(ppToPay.getStatus())) {
            throw new AppException(AppError.PAYMENT_OPTION_NOT_PAYABLE, ppToPay.getOrganizationFiscalCode(), iuv);
        }
        if (DebtPositionStatus.getPaymentPosFullyPaidStatus().contains(ppToPay.getStatus())) {
            throw new AppException(AppError.PAYMENT_OPTION_ALREADY_PAID, ppToPay.getOrganizationFiscalCode(), iuv);
        }
        // Verifico se la posizione debitoria è ancora aperta
        checkPaymentPositionOpen(ppToPay, iuv);
        // Verifico se l'opzione di pagamento è pagabile
        checkPaymentOptionPayable(ppToPay, iuv);
    }

    public static void checkPaymentPositionAccountability(PaymentPosition ppToReport, String iuv, String transferId) {
        // Verifico se la posizione debitoria è in uno stato idoneo alla rendicontazione
        if (DebtPositionStatus.getPaymentPosNotAccountableStatus().contains(ppToReport.getStatus())) {
            throw new AppException(AppError.TRANSFER_NOT_ACCOUNTABLE, ppToReport.getOrganizationFiscalCode(), iuv, transferId);
        }
        // Verifico se la transazione è rendicontabile
        checkTransferAccountable(ppToReport, iuv, transferId);
    }

    /*
     * return updated date interval
     */
    public static List<LocalDateTime> checkDatesInterval (LocalDateTime from, LocalDateTime to, int maxDaysInterval) {
        if(from != null && to == null)
            to = from.plus(maxDaysInterval, ChronoUnit.DAYS).with(LocalTime.MAX);
        else if(from == null && to != null)
            from = to.minus(maxDaysInterval, ChronoUnit.DAYS).with(LocalTime.MIN);

        if (from != null && to != null && (!(from.isBefore(to) || from.isEqual(to)) || Duration.between(from, to).toDays() > maxDaysInterval)) {
    		throw new AppException(AppError.DEBT_POSITION_NOT_RECOVERABLE, from, to, Duration.between(from, to).toDays(), maxDaysInterval);
    	}

        return Arrays.asList(from, to);
    }

    private static void checkPaymentPositionContentCongruency(final PaymentPosition pp) {

        LocalDateTime today = LocalDateTime.now(ZoneOffset.UTC);
        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss");

        // Regola 1 - must be validity_date ≥ current time
        if (null != pp.getValidityDate() && pp.getValidityDate().compareTo(today) < 0) {
            throw new ValidationException(
                    String.format(VALIDITY_DATE_VALIDATION_ERROR,
                            dateFormatter.format(pp.getValidityDate()),
                            dateFormatter.format(today)
                    )
            );
        }

        for (PaymentOption po : pp.getPaymentOption()) {
            // Regola 4 - must be due_date ≥ validity_date || due_date ≥ current time
            if (null != pp.getValidityDate() && po.getDueDate().compareTo(pp.getValidityDate()) < 0 ||
                    null == pp.getValidityDate() && po.getDueDate().compareTo(today) < 0) {
                throw new ValidationException(
                        String.format(DUE_DATE_VALIDATION_ERROR,
                                dateFormatter.format(po.getDueDate()),
                                (null != pp.getValidityDate() ? dateFormatter.format(pp.getValidityDate()) : null),
                                dateFormatter.format(today)
                        )
                );
            }
            // must be retention_date ≥ due_date
            else if (null != po.getRetentionDate() && po.getRetentionDate().compareTo(po.getDueDate()) < 0) {
                throw new ValidationException(
                        String.format(RETENTION_DATE_VALIDATION_ERROR,
                                dateFormatter.format(po.getRetentionDate()),
                                dateFormatter.format(po.getDueDate())
                        )
                );
            }

            chechPaymentOptionTransfers(po);

            checkPaymentOptionAmounts(po);
        }


    }

    private static void chechPaymentOptionTransfers(final PaymentOption po) {
        int maxNumberOfTrasfersForPO = TransferId.values().length;
        // verifica numero massimo di transazioni per PO
        if (po.getTransfer().size() > maxNumberOfTrasfersForPO) {
            throw new ValidationException(
                    String.format(NUM_TRANSFERS_VALIDATION_ERROR,
                            TransferId.values().length,
                            po.getTransfer().size()
                    )
            );
        }

        // verifica corretta valorizzazione idTransfer
        for (Transfer t : po.getTransfer()) {
            if (null == TransferId.fromValue(t.getIdTransfer())) {
                throw new ValidationException(
                        String.format(TRANSFER_ID_VALIDATION_ERROR,
                                t.getIdTransfer(),
                                Arrays.asList(TransferId.values())
                        )
                );
            }
        }
    }

    private static void checkPaymentOptionAmounts(final PaymentOption po) {
        long totalTranfersAmout = 0;
        long poAmount = po.getAmount();
        for (Transfer t : po.getTransfer()) {
            checkTransferCategory(t);
            checkTransferIban(t);
            totalTranfersAmout += t.getAmount();
        }

        if (poAmount != totalTranfersAmout) {
            throw new ValidationException(
                    String.format(AMOUNTS_VALIDATION_ERROR,
                            poAmount,
                            totalTranfersAmout
                    )
            );
        }

    }

    private static void checkTransferCategory(final Transfer t) {
        //TODO Da capire come validare il dato
        t.getCategory();
    }

    private static void checkTransferIban(final Transfer t) {
        //TODO Da capire come validare il dato
        t.getOrganizationFiscalCode();
        t.getIban();
    }

    private static void checkPaymentPositionOpen(PaymentPosition ppToPay, String iuv) {
        for (PaymentOption po : ppToPay.getPaymentOption()) {
            if (isPaid(po)) {
                throw new AppException(AppError.PAYMENT_OPTION_ALREADY_PAID, po.getOrganizationFiscalCode(), iuv);
            }
        }
    }

    private static void checkPaymentOptionPayable(PaymentPosition ppToPay, String iuv) {
        PaymentOption poToPay = ppToPay.getPaymentOption().stream().filter(po -> po.getIuv().equals(iuv)).findFirst()
                .orElseThrow(() -> {
                    log.error("Obtained unexpected empty payment option - ["
                            + String.format(LOG_BASE_PARAMS_DETAIL,
                            ppToPay.getOrganizationFiscalCode(),
                            ppToPay.getIupd(),
                            iuv
                    )
                            + "]");
                    throw new AppException(AppError.PAYMENT_OPTION_PAY_FAILED, ppToPay.getOrganizationFiscalCode(), iuv);
                });

        if (!poToPay.getStatus().equals(PaymentOptionStatus.PO_UNPAID)) {
            throw new AppException(AppError.PAYMENT_OPTION_ALREADY_PAID, poToPay.getOrganizationFiscalCode(), iuv);
        }

        // La posizione debitoria è già in PARTIALLY_PAID ed arriva una richiesta di pagamento su una payment option non rateizzata (isPartialPayment = false) => errore
        if (ppToPay.getStatus().equals(DebtPositionStatus.PARTIALLY_PAID) && Boolean.FALSE.equals(poToPay.getIsPartialPayment())) {
            throw new AppException(AppError.PAYMENT_OPTION_ALREADY_PAID, poToPay.getOrganizationFiscalCode(), iuv);
        }

    }

    private static boolean isPaid(PaymentOption po) {
        return !po.getStatus().equals(PaymentOptionStatus.PO_UNPAID) &&
                !po.getIsPartialPayment().equals(true);
    }

    private static void checkTransferAccountable(PaymentPosition ppToReport, String iuv, String transferId) {
        PaymentOption poToReport = ppToReport.getPaymentOption().stream().filter(po -> po.getIuv().equals(iuv)).findFirst()
                .orElseThrow(() -> {
                    log.error("Obtained unexpected empty payment option - ["
                            + String.format(LOG_BASE_PARAMS_DETAIL,
                            ppToReport.getOrganizationFiscalCode(),
                            ppToReport.getIupd(),
                            iuv
                    )
                            + "]");
                    throw new AppException(AppError.TRANSFER_REPORTING_FAILED, ppToReport.getOrganizationFiscalCode(), iuv, transferId);
                });

        if (!poToReport.getStatus().equals(PaymentOptionStatus.PO_PAID) && !poToReport.getStatus().equals(PaymentOptionStatus.PO_PARTIALLY_REPORTED)) {
            throw new AppException(AppError.TRANSFER_NOT_ACCOUNTABLE, poToReport.getOrganizationFiscalCode(), iuv, transferId);
        }

        Transfer transferToReport = poToReport.getTransfer().stream().filter(t -> t.getIdTransfer().equals(transferId)).findFirst()
                .orElseThrow(() -> {
                    log.error("Obtained unexpected empty transfer - ["
                            + String.format(LOG_BASE_PARAMS_DETAIL,
                            ppToReport.getOrganizationFiscalCode(),
                            ppToReport.getIupd(),
                            iuv
                    )
                            + "idTransfer= " + transferId
                            + "]");
                    throw new AppException(AppError.TRANSFER_REPORTING_FAILED, ppToReport.getOrganizationFiscalCode(), iuv, transferId);
                });


        if (!transferToReport.getStatus().equals(TransferStatus.T_UNREPORTED)) {
            throw new AppException(AppError.TRANSFER_NOT_ACCOUNTABLE, transferToReport.getOrganizationFiscalCode(), iuv, transferId);
        }

    }

    private enum TransferId {
        N1("1"), N2("2"), N3("3"), N4("4"), N5("5");

        private final String value;

        TransferId(String value) {
            this.value = value;
        }

        public static TransferId fromValue(String text) {
            for (TransferId t : TransferId.values()) {
                if (t.value.equalsIgnoreCase(text)) {
                    return t;
                }
            }
            return null;
        }

        @Override
        public String toString() {
            return value;
        }
    }

}
