package it.gov.pagopa.debtposition.service.common;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import java.util.List;
import java.util.Objects;

public final class PaymentConflictValidator {

  // Private constructor to prevent instantiation
  private PaymentConflictValidator() {
    throw new IllegalStateException("Utility class");
  }

  /**
   * Checks if the user is trying to pay the full amount for the payment position but there is an
   * installment already paid. Cross-payment guard executed on DB-locked siblings to avoid races.
   * Rules: - If parent is PAID or REPORTED -> block everything. - If paying a single option: block
   * when ANY sibling is already paid (installment or single). - If paying an installment: allow
   * ONLY if every already paid sibling belongs to the SAME plan.
   */
  public static void checkAlreadyPaidInstallments(
      PaymentOption poToPay, String nav, PaymentOptionRepository poRepo) {

    // Skip when current PO is not UNPAID (e.g. reporting flow or repeated updates)
    if (poToPay.getStatus() != PaymentOptionStatus.PO_UNPAID) {
      return;
    }

    PaymentPosition pp = poToPay.getPaymentPosition();
    if (pp == null || pp.getId() == null) {
      return; // no parent or not persisted yet
    }

    // 1) Lock ALL siblings so we read a consistent state and prevent concurrent cross-payments
    List<PaymentOption> siblingsLocked = poRepo.lockAllByPaymentPositionId(pp.getId());

    // 2) Hard guard on parent status
    if (pp.getStatus() == DebtPositionStatus.PAID
        || pp.getStatus() == DebtPositionStatus.REPORTED) {
      throw new AppException(
          AppError.PAYMENT_OPTION_NOT_FOUND, poToPay.getOrganizationFiscalCode(), nav);
    }

    boolean isFullPayment = !Boolean.TRUE.equals(poToPay.getIsPartialPayment());
    String planId = poToPay.getPaymentPlanId();

    // "Already paid" means status != PO_UNPAID
    java.util.function.Predicate<PaymentOption> isAlreadyPaid =
        po -> po.getStatus() != PaymentOptionStatus.PO_UNPAID;

    // CASE A: Attempting to pay Full Amount (Single Option)
    if (isFullPayment) {
      // Conflict if ANY sibling (single or installment) has already been paid
      boolean anyPaid = siblingsLocked.stream().anyMatch(isAlreadyPaid);
      if (anyPaid) {
        throw new AppException(
            AppError.PAYMENT_OPTION_NOT_FOUND, poToPay.getOrganizationFiscalCode(), nav);
      }
      return;
    }

    // CASE B: Attempting to pay an Installment
    // Conflict if:
    // 1. A Single Option (Full Payment) is already paid.
    // 2. An installment from a DIFFERENT plan is already paid.
    boolean conflict =
        siblingsLocked.stream()
            .filter(isAlreadyPaid)
            .anyMatch(
                paid -> {
                  boolean paidIsInstallment = Boolean.TRUE.equals(paid.getIsPartialPayment());
                  if (!paidIsInstallment) {
                    // single payment already paid -> conflict
                    return true;
                  }
                  // Conflict: Paid installment belongs to a different plan (if plan IDs differ)
                  return !Objects.equals(planId, paid.getPaymentPlanId());
                });

    if (conflict) {
      throw new AppException(
          AppError.PAYMENT_OPTION_NOT_FOUND, poToPay.getOrganizationFiscalCode(), nav);
    }
  }
}
