package it.gov.pagopa.debtposition.model.enumeration;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.EnumSet;
import java.util.Set;

public enum DebtPositionStatus {
    DRAFT, PUBLISHED, VALID, INVALID, EXPIRED, PARTIALLY_PAID, PAID, REPORTED;

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

    public static Set<DebtPositionStatus> getPaymentPosNotPayableStatus() {
        return EnumSet.of(DRAFT, PUBLISHED, INVALID, EXPIRED);
    }

    public static Set<DebtPositionStatus> getPaymentPosFullyPaidStatus() {
        return EnumSet.of(PAID, REPORTED);
    }

    public static Set<DebtPositionStatus> getPaymentPosNotAccountableStatus() {
        return EnumSet.of(DRAFT, PUBLISHED, VALID, INVALID, EXPIRED, REPORTED);
    }

    public static PaymentPosition validityCheckAndUpdate(PaymentPosition pp) {
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        // Validity check on the fly
        if(pp.getStatus().equals(DebtPositionStatus.PUBLISHED) && null != pp.getValidityDate() && currentDate.isAfter(pp.getValidityDate())) {
            pp.setStatus(DebtPositionStatus.VALID);
        }
        return pp;
    }

    public static PaymentPosition expirationCheckAndUpdate(PaymentPosition pp) {
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        // Expiration check on the fly
        if(pp.getSwitchToExpired() && pp.getStatus().equals(DebtPositionStatus.VALID)
                && null != pp.getMaxDueDate() && currentDate.isAfter(pp.getMaxDueDate())) {
            pp.setStatus(DebtPositionStatus.EXPIRED);
        }
        return pp;
    }

    public static PaymentPosition validityCheckAndUpdate(PaymentOption po) {
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        PaymentPosition pp = po.getPaymentPosition();
        // Validity check on the fly
        if(pp.getStatus().equals(DebtPositionStatus.PUBLISHED) && null != pp.getValidityDate() && currentDate.isAfter(pp.getValidityDate())) {
            pp.setStatus(DebtPositionStatus.VALID);
        }
        return pp;
    }

    public static PaymentPosition expirationCheckAndUpdate(PaymentOption po) {
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        // Expiration check on the fly
        PaymentPosition pp = po.getPaymentPosition();
        if(pp.getSwitchToExpired() && pp.getStatus().equals(DebtPositionStatus.VALID)
                && null != pp.getMaxDueDate() && currentDate.isAfter(pp.getMaxDueDate())) {
            pp.setStatus(DebtPositionStatus.EXPIRED);
        }
        return pp;
    }
}
