package it.gov.pagopa.debtposition.model.enumeration;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.util.CommonUtil;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

public enum DebtPositionStatus {
  DRAFT,
  PUBLISHED,
  VALID,
  INVALID,
  EXPIRED,
  PARTIALLY_PAID,
  PAID,
  REPORTED;

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
    // calculate the min validity date among the installments
    LocalDateTime minValidity = CommonUtil.resolveMinValidity(pp);
    // Validity check on the fly
    if (pp.getStatus().equals(DebtPositionStatus.PUBLISHED)
    		&& minValidity != null
    		&& currentDate.isAfter(minValidity)) {
    	pp.setStatus(DebtPositionStatus.VALID);
    }
    return pp;
  }
  
  public static PaymentPosition expirationCheckAndUpdate(PaymentPosition pp) {
	LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);

	// switchToExpired = true if at least one installment has switchToExpired = true
	boolean anySwitchToExpired = UtilityMapper.getSwitchToExpired(pp);

	if (anySwitchToExpired && pp.getStatus() == DebtPositionStatus.VALID && pp.getMaxDueDate() != null
			&& currentDate.isAfter(pp.getMaxDueDate())) {
		pp.setStatus(DebtPositionStatus.EXPIRED);
	}
	return pp;
  }

    /**
     * This method check for Installment validity. An Installment (payment option) could be valid from a specific datetime.
     * @param currentDate is the LocalDateTime at the moment of the check
     * @param po payment option is the installment (ie rata) associated with a notice number (NAV)
     * @return true if is before or equal to currentDate
     */
  public static boolean isInstallmentValid(LocalDateTime currentDate, PaymentOption po) {
      LocalDateTime validityDate = po.getValidityDate();
      return validityDate == null || !po.getValidityDate().isAfter(currentDate);
  }

  public static void validityCheckAndUpdate(LocalDateTime currentDate, PaymentOption po) {
    PaymentPosition pp = po.getPaymentPosition();
    LocalDateTime minValidity = CommonUtil.resolveMinValidity(pp);
    // Validity check on the fly
    if (pp.getStatus().equals(DebtPositionStatus.PUBLISHED)
    		&& minValidity != null
    		&& currentDate.isAfter(minValidity)) {
    	pp.setStatus(DebtPositionStatus.VALID);
    }
  }
  
  public static void expirationCheckAndUpdate(LocalDateTime currentDate, PaymentOption po) {

	  PaymentPosition pp = po.getPaymentPosition();

	  // PP must be in payable status, otherwise skip
	  if (pp.getStatus() != DebtPositionStatus.VALID
			  && pp.getStatus() != DebtPositionStatus.PARTIALLY_PAID) {
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
   * Checks if the user is trying to pay the full amount for the payment position but there is an
   * installment already paid.
   * Cross-payment guard executed on DB-locked siblings to avoid races.
   * Rules:
   *  - If parent is PAID or REPORTED -> block everything.
   *  - If paying a single option: block when ANY sibling is already paid (installment or single).
   *  - If paying an installment: allow ONLY if every already paid sibling belongs to the SAME plan.
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
    if (pp.getStatus() == DebtPositionStatus.PAID || pp.getStatus() == DebtPositionStatus.REPORTED) {
      throw new AppException(
          AppError.PAYMENT_OPTION_NOT_FOUND, poToPay.getOrganizationFiscalCode(), nav);
    }

    boolean isFullPayment = !Boolean.TRUE.equals(poToPay.getIsPartialPayment());
    String planId = poToPay.getPaymentPlanId();

    // "Already paid" means status != PO_UNPAID
    java.util.function.Predicate<PaymentOption> isAlreadyPaid =
        po -> po.getStatus() != PaymentOptionStatus.PO_UNPAID;

    if (isFullPayment) {
      // Single option cannot be paid if ANY sibling has already been paid
      boolean anyPaid = siblingsLocked.stream().anyMatch(isAlreadyPaid);
      if (anyPaid) {
        throw new AppException(
            AppError.PAYMENT_OPTION_NOT_FOUND, poToPay.getOrganizationFiscalCode(), nav);
      }
      return;
    }

    // Paying an installment:
    // there must be NO paid single option and NO paid installment from a DIFFERENT plan
    boolean conflict =
        siblingsLocked.stream()
            .filter(isAlreadyPaid)
            .anyMatch(paid -> {
              boolean paidIsInstallment = Boolean.TRUE.equals(paid.getIsPartialPayment());
              if (!paidIsInstallment) {
                // single payment already paid -> conflict
                return true;
              }
              // paid installment -> conflict if plan IDs differ
              return !Objects.equals(planId, paid.getPaymentPlanId());
            });

    if (conflict) {
      throw new AppException(
          AppError.PAYMENT_OPTION_NOT_FOUND, poToPay.getOrganizationFiscalCode(), nav);
    }
  }

}
