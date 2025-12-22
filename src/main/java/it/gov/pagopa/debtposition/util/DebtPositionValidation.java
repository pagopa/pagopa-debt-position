package it.gov.pagopa.debtposition.util;

import static it.gov.pagopa.debtposition.util.CommonUtil.sanitize;
import static it.gov.pagopa.debtposition.util.Constants.CREATE_ACTION;
import static it.gov.pagopa.debtposition.util.Constants.UPDATE_ACTION;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.exception.ValidationException;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.logging.log4j.util.Strings;

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
    checkPaymentPositionDatesCongruency(pp, action);
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
    checkPaymentOptionPayable(ppToPay, nav);
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

  // Validation based on validityDate, dueDate, retentionDate, currentDate
  private static void checkPaymentPositionDatesCongruency(final PaymentPosition pp, String... actions) {

    String action = ArrayUtils.isEmpty(actions) ? "NO_ACTION" : actions[0];
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss");

    for (PaymentOption po : pp.getPaymentOption()) {

      LocalDateTime poValidity = po.getValidityDate();

      // 1. Validity Date Check: in CREATE action, validity_date must be greater than current_time
      if (CREATE_ACTION.equalsIgnoreCase(action) && poValidity != null && !poValidity.isAfter(now)) {
        throw new ValidationException(
            String.format(
                VALIDITY_DATE_VALIDATION_ERROR,
                dateFormatter.format(poValidity),
                dateFormatter.format(now)));
      }

      // 2. Due Date Check: action is used as param to differentiate cases
      if (isDueDateInvalid(po, poValidity, now, action)) {
        throw new ValidationException(
            String.format(
                DUE_DATE_VALIDATION_ERROR,
                dateFormatter.format(po.getDueDate()),
                (poValidity != null ? dateFormatter.format(poValidity) : null),
                dateFormatter.format(now)));
      }

      // 3. Retention Date Check: regardless of the action, retention_date must be greater than due_date
      if (po.getRetentionDate() != null && po.getRetentionDate().isBefore(po.getDueDate())) {
        throw new ValidationException(
            String.format(
                RETENTION_DATE_VALIDATION_ERROR,
                dateFormatter.format(po.getRetentionDate()),
                dateFormatter.format(po.getDueDate())));
      }

      checkPaymentOptionTransfers(po);
      checkPaymentOptionAmounts(po);
    }
  }

  private static void checkPaymentOptionTransfers(final PaymentOption po) {
    int maxNumberOfTrasfersForPO = TransferId.values().length;
    // verifica numero massimo di transazioni per PO
    if (po.getTransfer().size() > maxNumberOfTrasfersForPO) {
      throw new ValidationException(
          String.format(
              NUM_TRANSFERS_VALIDATION_ERROR, TransferId.values().length, po.getTransfer().size()));
    }

    // verifica corretta valorizzazione idTransfer
    for (Transfer t : po.getTransfer()) {
      if (null == TransferId.fromValue(t.getIdTransfer())) {
        throw new ValidationException(
            String.format(
                TRANSFER_ID_VALIDATION_ERROR,
                t.getIdTransfer(),
                Arrays.asList(TransferId.values())));
      }
    }
  }

  private static void checkPaymentOptionAmounts(final PaymentOption po) {
    long totalTranfersAmout = 0;
    long poAmount = po.getAmount();
    for (Transfer t : po.getTransfer()) {
      checkMutualExclusive(po.getIuv(), t);
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
      else throw new ValidationException(String.format(IBAN_STAMP_MUTUAL, iuv, t.getIdTransfer()));
    }
    if (i != 1) {
      throw new ValidationException(String.format(IBAN_STAMP_MUTUAL, iuv, t.getIdTransfer()));
    }
  }

  private static void checkPaymentPositionOpen(PaymentPosition ppToPay, String nav) {
    for (PaymentOption po : ppToPay.getPaymentOption()) {
      if (!po.getStatus().equals(PaymentOptionStatus.PO_UNPAID)
          && po.getIsPartialPayment().equals(false)) {
        throw new AppException(
            AppError.PAYMENT_OPTION_ALREADY_PAID, po.getOrganizationFiscalCode(), nav);
      }
    }
  }

  private static void checkPaymentOptionPayable(PaymentPosition ppToPay, String nav) {
    // TODO #naviuv: temporary regression management --> remove "|| po.getIuv().equals(nav)" when
    // only nav managment is enabled
    PaymentOption poToPay =
        ppToPay.getPaymentOption().stream()
            .filter(po -> po.getNav().equals(nav) || po.getIuv().equals(nav))
            .findFirst()
            .orElseThrow(
                () -> {
                  logErrorEmptyPaymentOption(ppToPay, nav);
                  return new AppException(
                      AppError.PAYMENT_OPTION_PAY_FAILED, ppToPay.getOrganizationFiscalCode(), nav);
                });

    if (!poToPay.getStatus().equals(PaymentOptionStatus.PO_UNPAID)) {
      throw new AppException(
          AppError.PAYMENT_OPTION_ALREADY_PAID, poToPay.getOrganizationFiscalCode(), nav);
    }

    // La posizione debitoria è già in PARTIALLY_PAID ed arriva una richiesta di pagamento su una
    // payment option non rateizzata (isPartialPayment = false) => errore
    // PIDM-42: if this is a full payment and the position is partially paid then
    // log this but allow the payment option status to be changed to PO_PAID instead of throwing an
    // error
    // NOTE: the exception handling has been moved to the get/activate validation
    // (checkAlreadyPaidInstallments)
    if (ppToPay.getStatus().equals(DebtPositionStatus.PARTIALLY_PAID)
        && Boolean.FALSE.equals(poToPay.getIsPartialPayment())) {

      // log detailed information about this edge case
      log.warn(
          "Potential payment state inconsistency detected || "
              + "Organization: {} || "
              + "IUPD: {} || "
              + "NAV: {} || "
              + "Position Status: {} || "
              + "Payment Option Status: {} || "
              + "Is Partial Payment: {} || "
              + "Timestamp: {}",
          sanitize(ppToPay.getOrganizationFiscalCode()),
          sanitize(ppToPay.getIupd()),
          sanitize(nav),
          ppToPay.getStatus(),
          poToPay.getStatus(),
          poToPay.getIsPartialPayment(),
          LocalDateTime.now());
    }
  }

  private static void checkTransferAccountable(
      PaymentPosition ppToReport, String iuv, String transferId) {
    PaymentOption poToReport =
        ppToReport.getPaymentOption().stream()
            .filter(po -> po.getIuv().equals(iuv))
            .findFirst()
            .orElseThrow(
                () -> {
                  logErrorEmptyPaymentOption(ppToReport, iuv);
                  return new AppException(
                      AppError.TRANSFER_REPORTING_FAILED,
                      ppToReport.getOrganizationFiscalCode(),
                      iuv,
                      transferId);
                });

    if (!ppToReport.getServiceType().equals(ServiceType.ACA)
        && (!poToReport.getStatus().equals(PaymentOptionStatus.PO_PAID)
            && !poToReport.getStatus().equals(PaymentOptionStatus.PO_PARTIALLY_REPORTED))) {
      throw new AppException(
          AppError.TRANSFER_NOT_ACCOUNTABLE,
          poToReport.getOrganizationFiscalCode(),
          iuv,
          transferId);
    }

    Transfer transferToReport =
        poToReport.getTransfer().stream()
            .filter(t -> t.getIdTransfer().equals(transferId))
            .findFirst()
            .orElseThrow(
                () -> {
                  log.error(
                      "Obtained unexpected empty transfer - [{}idTransfer= {}]",
                      String.format(
                          LOG_BASE_PARAMS_DETAIL,
                          sanitize(ppToReport.getOrganizationFiscalCode()),
                          sanitize(ppToReport.getIupd()),
                          sanitize(iuv)),
                          sanitize(transferId));
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

  private static void logErrorEmptyPaymentOption(PaymentPosition pp, String iuv) {
    log.error(
        "Obtained unexpected empty payment option - [{}]",
        String.format(LOG_BASE_PARAMS_DETAIL, sanitize(pp.getOrganizationFiscalCode()), sanitize(pp.getIupd()), sanitize(iuv)));
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

  /* In CREATE, due_date must be greater than validity_date and current_time.
   * In UPDATE, due_date must be greater than validity_date. If switchToExpired is false,
   * due_date could be lower than current_time.
   */
  private static boolean isDueDateInvalid(PaymentOption po, LocalDateTime poValidityIn, LocalDateTime now, String action) {
    LocalDateTime validityDate = (poValidityIn == null) ? now : poValidityIn;
    boolean isInvalid = false;
    boolean isSTE = Boolean.TRUE.equals(po.getSwitchToExpired());

    // Regardless of the action, the due date must be later than the validity date.
    // Otherwise, when the citizen retrieves the option, they get an inconsistent response (validity date > due date).
    isInvalid = po.getDueDate().isBefore(validityDate);

    if (CREATE_ACTION.equalsIgnoreCase(action) || (UPDATE_ACTION.equalsIgnoreCase(action) && isSTE)) {
      // due_date must be greater than validity_date and current_time
      isInvalid = po.getDueDate().isBefore(now) || isInvalid;
    }

    // In Update action, if switchToExpired is false due_date could be lower than current_time

    return isInvalid;
  }
}
