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

public enum DebtPositionStatusV3 {
  DRAFT,
  PUBLISHED,
  VALID,
  UNPAYABLE,
  PARTIALLY_PAID,
  PAID;

  public static Set<DebtPositionStatusV3> getPaymentPosNotYetPaidStatus() {
    return EnumSet.of(DRAFT, PUBLISHED, VALID, UNPAYABLE);
  }

  public static Set<DebtPositionStatusV3> getPaymentPosAlreadyPaidStatus() {
    return EnumSet.complementOf((EnumSet<DebtPositionStatusV3>) getPaymentPosNotYetPaidStatus());
  }

  // PAGOPA-2459 - removed EXPIRED as non-updatable final state.
  public static Set<DebtPositionStatusV3> getPaymentPosNotUpdatableStatus() {
    return EnumSet.of(PARTIALLY_PAID, PAID, UNPAYABLE);
  }

  public static Set<DebtPositionStatusV3> getPaymentPosNotPublishableStatus() {
    return EnumSet.of(PUBLISHED, VALID, PARTIALLY_PAID, PAID, UNPAYABLE);
  }

  public static Set<DebtPositionStatusV3> getPaymentPosNotInvalidableStatus() {
    return EnumSet.of(DRAFT, PARTIALLY_PAID, PAID, UNPAYABLE);
  }

  public static Set<DebtPositionStatusV3> getPaymentPosFullyPaidStatus() {
    return EnumSet.of(PAID);
  }

  public static Set<DebtPositionStatusV3> getPaymentPosNotAccountableStatus() {
    return EnumSet.of(DRAFT, PUBLISHED, VALID, UNPAYABLE);
  }

  public static Set<DebtPositionStatusV3> getPaymentPosACANotAccountableStatus() {
    return EnumSet.of(DRAFT, PUBLISHED, UNPAYABLE);
  }

  public static PaymentPosition validityCheckAndUpdate(PaymentPosition pp) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    // Validity check on the fly
    if (pp.getStatus().equals(DebtPositionStatusV3.PUBLISHED)
            && null != pp.getValidityDate()
            && currentDate.isAfter(pp.getValidityDate())) {
      pp.setStatus(DebtPositionStatusV3.VALID);
    }
    return pp;
  }

  public static void expirationCheckAndUpdate(PaymentPosition pp) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    // Expiration check on the fly
    if (pp.getPaymentOption().stream().allMatch(po -> Boolean.TRUE.equals(po.getSwitchToExpired())) // TODO VERIFY
            && pp.getStatus().equals(DebtPositionStatusV3.VALID)
            && null != pp.getMaxDueDate()
            && currentDate.isAfter(pp.getMaxDueDate())) {
      pp.setStatus(DebtPositionStatusV3.UNPAYABLE);
    }
  }

  public static void validityCheckAndUpdate(Installment installment) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    PaymentPosition pp = installment.getPaymentPosition();
    // Validity check on the fly
    if (pp.getStatus().equals(DebtPositionStatusV3.PUBLISHED)
            && null != pp.getValidityDate()
            && currentDate.isAfter(pp.getValidityDate())) {
      pp.setStatus(DebtPositionStatusV3.VALID);
    }
  }

  public static void expirationCheckAndUpdate(Installment installment) {
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    // Expiration check on the fly
    PaymentPosition pp = installment.getPaymentPosition();
    PaymentOption po = installment.getPaymentOption();
    if (Boolean.TRUE.equals(po.getSwitchToExpired())
            && pp.getStatus().equals(DebtPositionStatusV3.VALID)
            && null != pp.getMaxDueDate()
            && currentDate.isAfter(pp.getMaxDueDate())) {
      pp.setStatus(DebtPositionStatusV3.UNPAYABLE);
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
            && paymentPosition.getStatus().equals(DebtPositionStatusV3.PARTIALLY_PAID)) { // TODO VERIFY

      throw new AppException(
              AppError.PAYMENT_OPTION_ALREADY_PAID,
              paymentOptionToPay.getOrganizationFiscalCode(),
              nav);
    }
  }
}
