package it.gov.pagopa.debtposition.service.common;

import static it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus.getPaymentPosPayableStatus;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

public final class ExpirationHandler {

  // Private constructor to prevent instantiation
  private ExpirationHandler() {
    throw new IllegalStateException("Utility class");
  }

  /**
   * Based on the state machine, it checks whether the payment position meets the criteria to move
   * to the EXPIRED state.
   *
   * <p>The transition to {@code DebtPositionStatus.EXPIRED} occurs only if:
   *
   * <ul>
   *   <li>All installments are marked to switch to expired.
   *   <li>The current status is {@code VALID}.
   *   <li>The max due date exists and is in the past relative to UTC now.
   * </ul>
   *
   * @param pp the PaymentPosition to check and potentially update.
   */
  public static void handlePaymentPositionExpirationLogic(PaymentPosition pp) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);

    // switchToExpired = true if all installment has switchToExpired = true
    boolean allSwitchToExpired = UtilityMapper.hasAllMarkedExpired(pp.getPaymentOption());

    if (allSwitchToExpired
        && pp.getStatus() == DebtPositionStatus.VALID
        && pp.getMaxDueDate() != null
        && currentDate.isAfter(pp.getMaxDueDate())) {
      pp.setStatus(DebtPositionStatus.EXPIRED);
    }
  }

  /**
   * Checks whether the expiration of a specific Payment Option (installment) should trigger the
   * expiration of the entire Payment Position based on the state machine.
   *
   * <p>The parent {@code PaymentPosition} transitions to {@code DebtPositionStatus.EXPIRED} only
   * if:
   *
   * <ul>
   *   <li>The position is currently in a payable status, i.e. {@code VALID} or {@code
   *       PARTIALLY_PAID}.
   *   <li>The specific payment option is {@code PO_UNPAID}.
   *   <li>The payment option is marked as switchable to expired ({@code switchToExpired} is true).
   *   <li>The due date of the option is strictly before the current date.
   * </ul>
   *
   * @param currentDate the reference date-time to check against (usually UTC now).
   * @param po the PaymentOption to evaluate.
   */
  public static void handleInstallmentExpirationLogic(LocalDateTime currentDate, PaymentOption po) {
    PaymentPosition pp = po.getPaymentPosition();

    // PP must be in payable status, otherwise skip
    if (getPaymentPosPayableStatus().contains(pp.getStatus())) {
      return;
    }

    // The PO must be UNPAID, otherwise skip
    if (po.getStatus() != PaymentOptionStatus.PO_UNPAID) {
      return;
    }

    // The PO must be able to expire, otherwise skip
    if (!Boolean.TRUE.equals(po.getSwitchToExpired())) {
      return;
    }

    // The PO has expired, otherwise skip
    if (!po.getDueDate().isBefore(currentDate)) {
      return;
    }

    // Payment attempt on expired PO → set status to EXPIRED
    pp.setStatus(DebtPositionStatus.EXPIRED);
  }

  /**
   * Checks if the payment option is overdue based solely on the due date.
   *
   * <p><strong>Note:</strong> Unlike other expiration checks, this method <strong>ignores</strong>
   * the {@code switchToExpired} flag. It performs a strict temporal comparison intended for APIs
   * like logic {@code verifyPaymentOptions}.
   *
   * @param po the PaymentOption to check.
   * @param now the reference date-time (usually UTC now).
   * @return {@code true} if the due date exists and is in the past; {@code false} if the due date
   *     is null (no expiration) or in the future.
   */
  public static boolean isPastDueDate(PaymentOption po, LocalDateTime now) {
    LocalDateTime due = po.getDueDate();
    if (due == null) {
      return false;
    }
    return now.isAfter(due);
  }
}
