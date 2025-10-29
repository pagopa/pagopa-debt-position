package it.gov.pagopa.debtposition.model.enumeration;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.repository.InstallmentRepository;

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

    public static void validityCheckAndUpdate(PaymentPosition pp) {
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        // Validity check on the fly
        if (pp.getStatus().equals(DebtPositionStatus.PUBLISHED)
                && null != pp.getValidityDate()
                && currentDate.isAfter(pp.getValidityDate())) {
            pp.setStatus(DebtPositionStatus.VALID);
            pp.getPaymentOption().forEach(po -> {
                if (currentDate.isAfter(po.getValidityDate())) {
                    po.getInstallment().forEach(inst -> {
                        if (inst.getStatus() == InstallmentStatus.DRAFT) {
                            inst.setStatus(InstallmentStatus.UNPAID);
                        }
                    });
                }
            });
        }
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
            pp.getPaymentOption().forEach(po -> {
                if (currentDate.isAfter(po.getValidityDate())) {
                    po.getInstallment().forEach(inst -> {
                        if (inst.getStatus() == InstallmentStatus.DRAFT) {
                            inst.setStatus(InstallmentStatus.UNPAID);
                        }
                    });
                }
            });
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
     * @param instToPay the installment being paid
     * @param nav       the identifier of the notice being paid
     */
    public static void checkAlreadyPaidInstallments(Installment instToPay, String nav, InstallmentRepository instRepo) {
        // TODO VERIFY METHOD
        // Skip when current Installment is not UNPAID (e.g. reporting flow or repeated updates)
        if (instToPay.getStatus() != InstallmentStatus.UNPAID) {
            return;
        }

        PaymentPosition pp = instToPay.getPaymentPosition();
        if (pp == null || pp.getId() == null) {
            return; // no parent or not persisted yet
        }

        // 1) Lock ALL siblings so we read a consistent state and prevent concurrent cross-payments
        List<Installment> siblingsLocked = instRepo.lockAllByPaymentPositionId(pp.getId());

        // 2) Hard guard on parent status
        if (pp.getStatus() == DebtPositionStatus.PAID || pp.getStatus() == DebtPositionStatus.REPORTED) {
            throw new AppException(
                    AppError.PAYMENT_OPTION_ALREADY_PAID, instToPay.getOrganizationFiscalCode(), nav);
        }

        PaymentOption paymentOption = instToPay.getPaymentOption();
        boolean isFullPayment = OptionType.OPZIONE_UNICA.equals(paymentOption.getOptionType());

        if (isFullPayment) {
            // Single option cannot be paid if ANY sibling has already been paid
            boolean anyPaid = siblingsLocked.stream().anyMatch(inst ->
                    InstallmentStatus.getInstallmentPaidStatus().contains(inst.getStatus()));
            if (anyPaid) {
                throw new AppException(
                        AppError.PAYMENT_OPTION_ALREADY_PAID, instToPay.getOrganizationFiscalCode(), nav);
            }
            return;
        }

        // Paying an installment:
        // there must be NO paid single option and NO paid installment from a DIFFERENT plan
        boolean conflict =
                siblingsLocked.stream()
                        .filter(inst -> InstallmentStatus.getInstallmentPaidStatus().contains(inst.getStatus()))
                        .anyMatch(paid -> {
                            PaymentOption paidOption = paid.getPaymentOption();
                            boolean paidIsInstallment = OptionType.OPZIONE_RATEALE.equals(paidOption.getOptionType());
                            if (!paidIsInstallment) {
                                // single payment already paid -> conflict
                                return true;
                            }
                            // paid installment -> conflict if option IDs differ
                            return !Objects.equals(paymentOption.getId(), paidOption.getId());
                        });

        if (conflict) {
            throw new AppException(
                    AppError.PAYMENT_OPTION_ALREADY_PAID, instToPay.getOrganizationFiscalCode(), nav);
        }
    }
}
