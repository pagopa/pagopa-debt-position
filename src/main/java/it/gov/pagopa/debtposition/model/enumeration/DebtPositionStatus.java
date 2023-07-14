package it.gov.pagopa.debtposition.model.enumeration;

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

    public static Set<DebtPositionStatus> getPaymentPosNotUpdatableStatus() {
        return EnumSet.of(INVALID, EXPIRED, PARTIALLY_PAID, PAID, REPORTED);
    }

    public static Set<DebtPositionStatus> getPaymentPosNotPublishableStatus() {
        return EnumSet.of(PUBLISHED, VALID, INVALID, EXPIRED, PARTIALLY_PAID, PAID, REPORTED);
    }

    public static Set<DebtPositionStatus> getPaymentPosNotIvalidableStatus() {
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
}
