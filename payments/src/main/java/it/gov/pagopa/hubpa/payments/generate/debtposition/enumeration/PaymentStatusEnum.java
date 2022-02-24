package it.gov.pagopa.hubpa.payments.generate.debtposition.enumeration;

/**
 * Enumeration of debt position statuses:
 * <ul>
 * <li>PAYABLE - debt position is payable
 * <li>NOT_PAYABLE - debt position is no longer payable
 * <li>CANCELED - debt position is no longer valid
 * <li>PAID - debt position is paid
 * </ul>
 * 
 * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail
 * @see pagopa.gov.it.toolkit.debtPositionGenerator.DebtPositionManagement
 */
public enum PaymentStatusEnum {
    PAYABLE(1), NOT_PAYABLE(2), CANCELED(3), PAID(4);

    private final Integer value;

    /**
     * Extracts the corresponding enum value from an integer value
     * 
     * @param v
     *            a value
     * @return the enum value
     */
    public static PaymentStatusEnum fromValue(Integer v) {
        for (PaymentStatusEnum enumValue : PaymentStatusEnum.values()) {
            if (enumValue.value.equals(v)) {
                return enumValue;
            }
        }
        throw new IllegalArgumentException(String.valueOf(v));
    }

    /**
     * @param v
     */
    private PaymentStatusEnum(Integer v) {
        value = v;
    }

    /**
     * @return the value
     */
    public int value() {
        return value;
    }
}
