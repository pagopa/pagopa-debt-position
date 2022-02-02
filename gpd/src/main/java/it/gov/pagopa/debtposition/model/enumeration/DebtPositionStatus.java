package it.gov.pagopa.debtposition.model.enumeration;

import java.util.EnumSet;

public enum DebtPositionStatus {
    DRAFT, PUBLISHED, VALID, INVALID, EXPIRED, PARTIALLY_PAID, PAID, PARTIALLY_REPORTED, REPORTED;
	
	public static EnumSet<DebtPositionStatus> getPaymentPosNotYetPaidStatus() {
        return EnumSet.of(DRAFT, PUBLISHED, VALID, INVALID, EXPIRED);
    }
	
	public static EnumSet<DebtPositionStatus> getPaymentPosAlreadyPaidStatus() {
        return EnumSet.complementOf(getPaymentPosNotYetPaidStatus());
    }
}
