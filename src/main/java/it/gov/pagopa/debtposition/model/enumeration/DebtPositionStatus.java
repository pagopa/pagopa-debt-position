package it.gov.pagopa.debtposition.model.enumeration;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.EnumSet;
import java.util.Set;

public enum DebtPositionStatus {
  DRAFT,
  PUBLISHED,
  VALID,
  PARTIALLY_PAID,
  PAID,
  REPORTED,
  EXPIRED,
  INVALID;

  public static Set<DebtPositionStatus> getPaymentPosNotYetPaidStatus() {
    return EnumSet.of(DRAFT, PUBLISHED, VALID, INVALID, EXPIRED);
  }

  public static Set<DebtPositionStatus> getPaymentPosAlreadyPaidStatus() {
    return EnumSet.complementOf((EnumSet<DebtPositionStatus>) getPaymentPosNotYetPaidStatus());
  }

  // PAGOPA-2459 - removed EXPIRED as non-updatable final state.
  public static Set<DebtPositionStatus> getPaymentPosNotUpdatableStatus() {
    return EnumSet.of(INVALID, PARTIALLY_PAID, PAID, REPORTED);
  }

  public static Set<DebtPositionStatus> getPaymentPosNotPublishableStatus() {
    return EnumSet.of(PUBLISHED, VALID, INVALID, EXPIRED, PARTIALLY_PAID, PAID, REPORTED);
  }

  public static Set<DebtPositionStatus> getPaymentPosNotInvalidableStatus() {
    return EnumSet.of(DRAFT, INVALID, EXPIRED, PARTIALLY_PAID, PAID, REPORTED);
  }

  public static Set<DebtPositionStatus> getPaymentPosFullyPaidStatus() {
    return EnumSet.of(PAID, REPORTED);
  }

  public static Set<DebtPositionStatus> getPaymentPosNotAccountableStatus() {
    return EnumSet.of(DRAFT, PUBLISHED, VALID, INVALID, EXPIRED, REPORTED);
  }

  public static Set<DebtPositionStatus> getPaymentPosACANotAccountableStatus() {
    return EnumSet.of(DRAFT, PUBLISHED, INVALID, EXPIRED, REPORTED);
  }

  public static PaymentPosition validityCheckAndUpdate(PaymentPosition pp) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    // Validity check on the fly
    if (pp.getStatus().equals(DebtPositionStatus.PUBLISHED)
            && null != pp.getValidityDate()
            && currentDate.isAfter(pp.getValidityDate())) {
      pp.setStatus(DebtPositionStatus.VALID);
    }
    return pp;
  }

  public static void expirationCheckAndUpdate(PaymentPosition pp) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    // Expiration check on the fly
    if (pp.getPaymentOption().stream().anyMatch(po -> Boolean.TRUE.equals(po.getSwitchToExpired()))
            && pp.getStatus().equals(DebtPositionStatus.VALID)
            && null != pp.getMaxDueDate()
            && currentDate.isAfter(pp.getMaxDueDate())) {
      pp.setStatus(DebtPositionStatus.EXPIRED);
    }
  }

  public static void validityCheckAndUpdate(Installment installment) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    PaymentPosition pp = installment.getPaymentPosition();
    // Validity check on the fly
    if (pp.getStatus().equals(DebtPositionStatus.PUBLISHED)
            && null != pp.getValidityDate()
            && currentDate.isAfter(pp.getValidityDate())) {
      pp.setStatus(DebtPositionStatus.VALID);
    }
  }

  public static void expirationCheckAndUpdate(Installment installment) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    // Expiration check on the fly
    PaymentPosition pp = installment.getPaymentPosition();
    PaymentOption po = installment.getPaymentOption();
    if (Boolean.TRUE.equals(po.getSwitchToExpired())
            && pp.getStatus().equals(DebtPositionStatus.VALID)
            && null != pp.getMaxDueDate()
            && currentDate.isAfter(pp.getMaxDueDate())) {
      pp.setStatus(DebtPositionStatus.EXPIRED);
    }
  }

  /**
   * Checks if the user is trying to pay the full amount for the payment position but there is an
   * installment already paid, in which case
   *
   * @param paymentOptionToPay the payment option being paid
   * @param nav the identifier of the notice being paid
   */
  public static void checkAlreadyPaidInstallments(PaymentOption paymentOptionToPay, String nav) {

    PaymentPosition paymentPosition = paymentOptionToPay.getPaymentPosition();
    if (OptionType.OPZIONE_UNICA.equals(paymentOptionToPay.getOptionType())
            && paymentPosition.getStatus().equals(DebtPositionStatus.PARTIALLY_PAID)) { // TODO VERIFY

      throw new AppException(
              AppError.PAYMENT_OPTION_ALREADY_PAID,
              paymentOptionToPay.getOrganizationFiscalCode(),
              nav);
    }
  }
}
