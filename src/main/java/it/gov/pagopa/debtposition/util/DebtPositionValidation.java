package it.gov.pagopa.debtposition.util;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.exception.ValidationException;
import it.gov.pagopa.debtposition.model.enumeration.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.logging.log4j.util.Strings;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.List;

import static it.gov.pagopa.debtposition.util.Constants.CREATE_ACTION;
import static it.gov.pagopa.debtposition.util.Constants.UPDATE_ACTION;

@Slf4j
public class DebtPositionValidation {

    private static final String LOG_BASE_PARAMS_DETAIL =
            "organizationFiscalCode= %s; iupd= %s; iuv= %s";
    private static final String DUE_DATE_VALIDATION_ERROR =
            "Dates congruence error: due_date must be >= validity_date (if provided) or due_date must be"
                    + " >= current_date [due_date=%s; validity_date=%s; current_date=%s]";
    private static final String RETENTION_DATE_VALIDATION_ERROR =
            "Dates congruence error: retention_date must be >= due_date [retention_date=%s; due_date=%s]"
                    + " ";
    private static final String VALIDITY_DATE_VALIDATION_ERROR =
            "Dates congruence error: validity_date must be >= current_date [validity_date=%s;"
                    + " current_date=%s] ";
    private static final String AMOUNTS_VALIDATION_ERROR =
            "Amounts congruence error: payment option amount must coincide with the total of the"
                    + " transfers amount [payment_option_amount(in cent)=%s; total_tranfers_amount(in"
                    + " cent)=%s]";
    private static final String NUM_TRANSFERS_VALIDATION_ERROR =
            "Number of transfers congruence error: Each payment option must have a maximum of %s"
                    + " transactions [transactions found=%s]";
    private static final String TRANSFER_ID_VALIDATION_ERROR =
            "Transfer ID congruence error: The transaction id not have a value between those expected"
                    + " [transaction id=%s; expected values=%s]";
    private static final String IBAN_STAMP_MUTUAL =
            "The Iban may be present (optionally combined with the Postal Iban) or the Stamp. Not all at"
                    + " the same time [iuv=%s, transaction id=%s].";

    private DebtPositionValidation() {
        super();
    }

    // PAGOPA-2459 - optional action parameter to specify checks based on create or update mode.
    public static void checkPaymentPositionInputDataAccuracy(PaymentPosition pp, String... action) {
        checkPaymentPositionContentCongruency(pp, action);
    }

    public static void checkPaymentPositionPayability(PaymentPosition ppToPay, String nav) {
        // Verifico se la posizione debitoria è in uno stato idoneo al pagamento
        if (DebtPositionStatus.getPaymentPosFullyPaidStatus().contains(ppToPay.getStatus())) {
            throw new AppException(
                    AppError.PAYMENT_OPTION_ALREADY_PAID, ppToPay.getOrganizationFiscalCode(), nav);
        }
        // Verifico se la posizione debitoria è ancora aperta
        checkPaymentPositionOpen(ppToPay, nav);
        // Verifico se l'opzione di pagamento è pagabile
        checkInstallmentPayable(ppToPay, nav);
    }

    public static void checkPaymentPositionAccountability(
            PaymentPosition ppToReport, String iuv, String transferId) {
        // Verifico se la posizione debitoria è in uno stato idoneo alla rendicontazione
        if (ppToReport.getServiceType().equals(ServiceType.ACA)) {
            if (DebtPositionStatus.getPaymentPosACANotAccountableStatus()
                    .contains(ppToReport.getStatus())) {
                throw new AppException(
                        AppError.TRANSFER_NOT_ACCOUNTABLE,
                        ppToReport.getOrganizationFiscalCode(),
                        iuv,
                        transferId);
            }
        } else {
            if (DebtPositionStatus.getPaymentPosNotAccountableStatus().contains(ppToReport.getStatus())) {
                throw new AppException(
                        AppError.TRANSFER_NOT_ACCOUNTABLE,
                        ppToReport.getOrganizationFiscalCode(),
                        iuv,
                        transferId);
            }
        }
        // Verifico se la transazione è rendicontabile
        checkTransferAccountable(ppToReport, iuv, transferId);
    }

    /*
     * return updated date interval
     */
    public static List<LocalDateTime> checkDatesInterval(
            LocalDateTime from, LocalDateTime to, int maxDaysInterval) {
        if (from != null && to == null)
            to = from.plus(maxDaysInterval, ChronoUnit.DAYS).with(LocalTime.MAX);
        else if (from == null && to != null)
            from = to.minus(maxDaysInterval, ChronoUnit.DAYS).with(LocalTime.MIN);

        if (from != null
                && to != null
                && (!(from.isBefore(to) || from.isEqual(to))
                || Duration.between(from, to).toDays() > maxDaysInterval)) {
            throw new AppException(
                    AppError.DEBT_POSITION_NOT_RECOVERABLE,
                    from,
                    to,
                    Duration.between(from, to).toDays(),
                    maxDaysInterval);
        }

        return Arrays.asList(from, to);
    }

    private static void checkPaymentPositionContentCongruency(
            final PaymentPosition pp, String... action) {

        LocalDateTime today = LocalDateTime.now(ZoneOffset.UTC);
        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss");

        // Regola 1 - must be validity_date ≥ current time (applied only at creation stage)
        if (!ArrayUtils.isEmpty(action)
                && action[0].equalsIgnoreCase(CREATE_ACTION)
                && null != pp.getValidityDate()
                && pp.getValidityDate().compareTo(today) < 0) {
            throw new ValidationException(
                    String.format(
                            VALIDITY_DATE_VALIDATION_ERROR,
                            dateFormatter.format(pp.getValidityDate()),
                            dateFormatter.format(today)));
        }

        for (PaymentOption po : pp.getPaymentOption()) {
            for (Installment inst : po.getInstallment()) {
                // Regola 4 - must be due_date ≥ validity_date || due_date ≥ current time
                if (
                    // Case 1: validity_date is not null and due_date < validity_date
                        (pp.getValidityDate() != null && inst.getDueDate().isBefore(pp.getValidityDate()))
                                ||

                                // Case 2: validity_date is null and due_date < current time
                                (pp.getValidityDate() == null && inst.getDueDate().isBefore(today))
                                ||

                                // Case 3: Action is "update" and due_date < current time
                                (!ArrayUtils.isEmpty(action)
                                        && UPDATE_ACTION.equalsIgnoreCase(action[0])
                                        && inst.getDueDate().isBefore(today))) {
                    throw new ValidationException(
                            String.format(
                                    DUE_DATE_VALIDATION_ERROR,
                                    dateFormatter.format(inst.getDueDate()),
                                    (null != pp.getValidityDate() ? dateFormatter.format(pp.getValidityDate()) : null),
                                    dateFormatter.format(today)));
                }
                // must be retention_date ≥ due_date
                else if (null != po.getRetentionDate()
                        && po.getRetentionDate().isBefore(inst.getDueDate())) {
                    throw new ValidationException(
                            String.format(
                                    RETENTION_DATE_VALIDATION_ERROR,
                                    dateFormatter.format(po.getRetentionDate()),
                                    dateFormatter.format(inst.getDueDate())));
                }

                checkInstallmentTransfers(inst);

                checkInstalmmentAmounts(inst);
            }
        }
    }

    private static void checkInstallmentTransfers(Installment inst) {
        int maxNumberOfTrasfersForPO = TransferId.values().length;
        // verifica numero massimo di transazioni per PO
        if (inst.getTransfer().size() > maxNumberOfTrasfersForPO) {
            throw new ValidationException(
                    String.format(
                            NUM_TRANSFERS_VALIDATION_ERROR, TransferId.values().length, inst.getTransfer().size()));
        }

        // verifica corretta valorizzazione idTransfer
        for (Transfer t : inst.getTransfer()) {
            if (null == TransferId.fromValue(t.getTransferId())) {
                throw new ValidationException(
                        String.format(
                                TRANSFER_ID_VALIDATION_ERROR,
                                t.getTransferId(),
                                Arrays.asList(TransferId.values())));
            }
        }
    }

    private static void checkInstalmmentAmounts(Installment inst) {
        long totalTranfersAmout = 0;
        long poAmount = inst.getAmount();
        for (Transfer t : inst.getTransfer()) {
            checkMutualExclusive(inst.getIuv(), t);
            totalTranfersAmout += t.getAmount();
        }

        if (poAmount != totalTranfersAmout) {
            throw new ValidationException(
                    String.format(AMOUNTS_VALIDATION_ERROR, poAmount, totalTranfersAmout));
        }
    }

    private static void checkMutualExclusive(String iuv, Transfer t) {
        int i = 0;
        if (Strings.isNotEmpty(t.getIban())) {
            i++;
        }
        if (Strings.isNotEmpty(t.getStampType())
                && Strings.isNotEmpty(t.getHashDocument())
                && Strings.isNotEmpty(t.getProvincialResidence())) {

            if (Strings.isEmpty(t.getPostalIban())) i++;
            else throw new ValidationException(String.format(IBAN_STAMP_MUTUAL, iuv, t.getTransferId()));
        }
        if (i != 1) {
            throw new ValidationException(String.format(IBAN_STAMP_MUTUAL, iuv, t.getTransferId()));
        }
    }

    private static void checkPaymentPositionOpen(PaymentPosition ppToPay, String nav) {
        for (PaymentOption po : ppToPay.getPaymentOption()) {
            if (po.getInstallment().stream().allMatch(inst -> InstallmentStatus.getInstallmentPaidStatus().contains(inst.getStatus()))) {
                throw new AppException(
                        AppError.PAYMENT_OPTION_ALREADY_PAID, po.getOrganizationFiscalCode(), nav);
            }
        }
    }

    private static void checkInstallmentPayable(PaymentPosition ppToPay, String nav) {
        // TODO #naviuv: temporary regression management --> remove "|| inst.getIuv().equals(nav)" when
        // only nav managment is enabled
        // TODO optimize & change errors
        Installment installmentToPay = ppToPay.getPaymentOption().parallelStream()
                .map(PaymentOption::getInstallment)
                .flatMap(List::parallelStream)
                .filter(inst -> inst.getNav().equals(nav) || inst.getIuv().equals(nav))
                .findFirst()
                .orElseThrow(
                        () -> {
                            log.error(
                                    "Obtained unexpected empty payment option - ["
                                            + String.format(
                                            LOG_BASE_PARAMS_DETAIL,
                                            CommonUtil.sanitize(ppToPay.getOrganizationFiscalCode()),
                                            CommonUtil.sanitize(ppToPay.getIupd()),
                                            CommonUtil.sanitize(nav))
                                            + "]");
                            return new AppException(
                                    AppError.PAYMENT_OPTION_PAY_FAILED, ppToPay.getOrganizationFiscalCode(), nav);
                        });

        if (InstallmentStatus.getInstallmentPaidStatus().contains(installmentToPay.getStatus())) {
            throw new AppException(
                    AppError.PAYMENT_OPTION_ALREADY_PAID, installmentToPay.getOrganizationFiscalCode(), nav);
        }

        // PIDM-42: if this is a full payment and the position is partially paid then
        // log this but allow the installment status to be changed to PAID instead of throwing an
        // error
        // NOTE: the exception handling has been moved to the get/activate validation
        // (checkAlreadyPaidInstallments)
        if (ppToPay.getStatus().equals(DebtPositionStatus.PARTIALLY_PAID)
                && OptionType.OPZIONE_UNICA.equals(installmentToPay.getPaymentOption().getOptionType())) {

            // log detailed information about this edge case
            log.warn(
                    "Potential payment state inconsistency detected || "
                            + "Organization: {} || "
                            + "IUPD: {} || "
                            + "NAV: {} || "
                            + "Position Status: {} || "
                            + "Payment Option Status: {} || "
                            + "Option type: {} || "
                            + "Timestamp: {}",
                    CommonUtil.sanitize(ppToPay.getOrganizationFiscalCode()),
                    CommonUtil.sanitize(ppToPay.getIupd()),
                    CommonUtil.sanitize(nav),
                    ppToPay.getStatus(),
                    installmentToPay.getStatus(),
                    installmentToPay.getPaymentOption().getOptionType(),
                    LocalDateTime.now());
        }
    }

    private static void checkTransferAccountable(
            PaymentPosition ppToReport, String iuv, String transferId) {
        // TODO change error
        Installment instToReport = ppToReport.getPaymentOption().stream()
                .map(PaymentOption::getInstallment)
                .flatMap(List::stream)
                .filter(inst -> inst.getIuv().equals(iuv))
                .findFirst()
                .orElseThrow(
                        () -> {
                            log.error(
                                    "Obtained unexpected empty payment option - ["
                                            + String.format(
                                            LOG_BASE_PARAMS_DETAIL,
                                            ppToReport.getOrganizationFiscalCode(),
                                            ppToReport.getIupd(),
                                            iuv)
                                            + "]");
                            return new AppException(
                                    AppError.TRANSFER_REPORTING_FAILED,
                                    ppToReport.getOrganizationFiscalCode(),
                                    iuv,
                                    transferId);
                        });

        if (!ppToReport.getServiceType().equals(ServiceType.ACA)
                && InstallmentStatus.getInstallmentTransferNotAccountableStatus().contains(instToReport.getStatus())) {
            throw new AppException(
                    AppError.TRANSFER_NOT_ACCOUNTABLE,
                    instToReport.getOrganizationFiscalCode(),
                    iuv,
                    transferId);
        }

        Transfer transferToReport =
                instToReport.getTransfer().stream()
                        .filter(t -> t.getTransferId().equals(transferId))
                        .findFirst()
                        .orElseThrow(
                                () -> {
                                    log.error(
                                            "Obtained unexpected empty transfer - ["
                                                    + String.format(
                                                    LOG_BASE_PARAMS_DETAIL,
                                                    ppToReport.getOrganizationFiscalCode(),
                                                    ppToReport.getIupd(),
                                                    iuv)
                                                    + "idTransfer= "
                                                    + transferId
                                                    + "]");
                                    return new AppException(
                                            AppError.TRANSFER_REPORTING_FAILED,
                                            ppToReport.getOrganizationFiscalCode(),
                                            iuv,
                                            transferId);
                                });

        if (!transferToReport.getStatus().equals(TransferStatus.T_UNREPORTED)) {
            throw new AppException(
                    AppError.TRANSFER_NOT_ACCOUNTABLE,
                    transferToReport.getOrganizationFiscalCode(),
                    iuv,
                    transferId);
        }
    }

    private enum TransferId {
        N1("1"),
        N2("2"),
        N3("3"),
        N4("4"),
        N5("5");

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
