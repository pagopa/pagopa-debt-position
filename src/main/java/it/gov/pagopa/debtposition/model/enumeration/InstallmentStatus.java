package it.gov.pagopa.debtposition.model.enumeration;

import java.util.EnumSet;
import java.util.Set;

public enum InstallmentStatus {
    DRAFT,
    UNPAID,
    PAID,
    PARTIALLY_REPORTED,
    REPORTED,
    UNPAYABLE,
    INVALID,
    EXPIRED;

    public static Set<InstallmentStatus> getInstallmentNotYetPaidStatus() {
        return EnumSet.of(DRAFT, UNPAID, UNPAYABLE, INVALID, EXPIRED);
    }

    public static Set<InstallmentStatus> getInstallmentPaidStatus() {
        return EnumSet.of(PAID, PARTIALLY_REPORTED, REPORTED);
    }

    public static Set<InstallmentStatus> getInstallmentNotUpdatableStatus() {
        return EnumSet.of(INVALID, UNPAYABLE, PAID, PARTIALLY_REPORTED, REPORTED, EXPIRED);
    }

    public static Set<InstallmentStatus> getInstallmentUpdatableStatus() {
        return EnumSet.of(DRAFT, UNPAID);
    }

    public static Set<InstallmentStatus> getInstallmentNotInvalidableStatus() {
        return EnumSet.of(DRAFT, PAID, PARTIALLY_REPORTED, REPORTED, UNPAYABLE, INVALID, EXPIRED);
    }

    public static Set<InstallmentStatus> getInstallmentNotAccountableStatus() {
        return EnumSet.of(DRAFT, PAID, PARTIALLY_REPORTED, REPORTED, INVALID, EXPIRED, UNPAYABLE);
    }

    public static Set<InstallmentStatus> getInstallmentTransferNotAccountableStatus() {
        return EnumSet.of(DRAFT, REPORTED, INVALID, EXPIRED, UNPAYABLE);
    }
}
