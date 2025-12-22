package it.gov.pagopa.debtposition.service.common;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.util.CommonUtil;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

public final class ValidityHandler {

  // Private constructor to prevent instantiation
  private ValidityHandler() {
    throw new IllegalStateException("Utility class");
  }

  /**
   * Checks if the Payment Position has reached its validity start date and updates its status.
   *
   * <p>The transition from {@code DEBT_POSITION_STATUS.PUBLISHED} to {@code
   * DEBT_POSITION_STATUS.VALID} occurs only if:
   *
   * <ul>
   *   <li>The current status is {@code PUBLISHED}.
   *   <li>A minimum validity date is defined across installments.
   *   <li>The current UTC time is after the calculated minimum validity date.
   * </ul>
   *
   * @param pp the PaymentPosition to check and potentially activate.
   */
  public static void handlePaymentPositionValidTransition(PaymentPosition pp) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    // calculate the min validity date among the installments
    LocalDateTime minValidity = CommonUtil.resolveMinValidity(pp);

    // Validity check on the fly
    if (pp.getStatus() == DebtPositionStatus.PUBLISHED
        && minValidity != null
        && currentDate.isAfter(minValidity)) {
      pp.setStatus(DebtPositionStatus.VALID);
    }
  }

  /**
   * Checks if a specific Payment Option (Installment) is currently valid based on its validity
   * date.
   *
   * <p>An installment is considered valid if:
   *
   * <ul>
   *   <li>It has no validity date (always valid).
   *   <li>OR the validity date is in the past or equal to the current date.
   * </ul>
   *
   * @param currentDate the reference date-time to check against.
   * @param po the PaymentOption (installment) to check.
   * @return {@code true} if the option is valid, {@code false} otherwise.
   */
  public static boolean isInstallmentValid(LocalDateTime currentDate, PaymentOption po) {
    LocalDateTime validityDate = po.getValidityDate();
    // Valid if date is null OR if current date is >= validityDate
    return validityDate == null || !validityDate.isAfter(currentDate);
  }
}
