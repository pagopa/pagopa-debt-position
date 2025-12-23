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
   * Evaluates the state of the Payment Position and updates it to {@code EXPIRED} if all criteria are met.
   *
   * <p>The transition to {@code DebtPositionStatus.EXPIRED} occurs only if:
   * <ul>
   * <li>All installments are marked to switch to expired ({@code switchToExpired} is true).</li>
   * <li>The current status is {@code VALID}.</li>
   * <li>The max due date exists and is in the past relative to the current UTC time.</li>
   * </ul>
   *
   * <p><b>Persistence Note (Dirty Checking):</b><br>
   * This method modifies the {@code status} attribute of the {@code PaymentPosition} entity.
   * If this method is invoked within an active {@code @Transactional} context and the entity is
   * <em>managed</em> (loaded within the current transaction), the JPA provider will automatically
   * detect the change and execute an {@code UPDATE} statement on the database upon transaction commit.
   * An explicit call to {@code repository.save(pp)} is not required.
   *
   * @param pp the PaymentPosition to check and potentially update.
   */
  public static boolean handlePaymentPositionExpirationLogic(PaymentPosition pp) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);

    // Check if all installments have switchToExpired = true
    boolean allSwitchToExpired = UtilityMapper.hasAllMarkedExpired(pp.getPaymentOption());

    if (allSwitchToExpired
        && pp.getStatus() == DebtPositionStatus.VALID
        && pp.getMaxDueDate() != null
        && currentDate.isAfter(pp.getMaxDueDate())) {
      pp.setStatus(DebtPositionStatus.EXPIRED);
      return true;
    }

    return false;
  }

  /**
   * Determines if a specific Payment Option (installment) meets the business criteria to be considered expired.
   *
   * <p>An installment is considered effectively expired if:
   * <ul>
   * <li>The parent position is in a payable status ({@code VALID} or {@code PARTIALLY_PAID}).</li>
   * <li>The installment status is {@code PO_UNPAID}.</li>
   * <li>The {@code switchToExpired} flag is explicitly set to {@code true}.</li>
   * <li>The due date is strictly before the current reference date.</li>
   * </ul>
   *
   * <p>Note: This method is a pure check (predicate) and does not modify the entity state.
   *
   * @param currentDate the reference date-time to check against (usually UTC now).
   * @param po the PaymentOption to evaluate.
   * @return {@code true} if the installment is considered expired; {@code false} otherwise.
   */
  public static boolean isInstallmentExpired(LocalDateTime currentDate, PaymentOption po) {
    PaymentPosition pp = po.getPaymentPosition();

    // PP must be in payable status, otherwise skip
    if (!getPaymentPosPayableStatus().contains(pp.getStatus())) {
      return false;
    }

    // The PO must be UNPAID, otherwise skip
    if (po.getStatus() != PaymentOptionStatus.PO_UNPAID) {
      return false;
    }

    // The PO must be able to expire, otherwise skip
    if (!Boolean.TRUE.equals(po.getSwitchToExpired())) {
      return false;
    }

    // The PO has expired, otherwise skip
    return po.getDueDate().isBefore(currentDate);
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