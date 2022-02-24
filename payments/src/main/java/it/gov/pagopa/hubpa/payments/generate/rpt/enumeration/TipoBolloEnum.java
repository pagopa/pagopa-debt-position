package it.gov.pagopa.hubpa.payments.generate.rpt.enumeration;

/**
 * Enumeration of tipoBollo:
 * <ul>
 * <li>IMPOSTA_DI_BOLLO - default value
 * </ul>
 * 
 * @see pagopa.gov.it.toolkit.common.bean.DatiMarcaBolloDigitale
 */
public enum TipoBolloEnum {
    IMPOSTA_DI_BOLLO("01");

    private final String value;

    /**
     * Extracts the corresponding enum value from a string value
     * 
     * @param v
     *            a value
     * @return the enum value
     */
    public static TipoBolloEnum fromValue(String v) {
        for (TipoBolloEnum enumValue : TipoBolloEnum.values()) {
            if (enumValue.value.equals(v)) {
                return enumValue;
            }
        }
        throw new IllegalArgumentException(v);
    }

    /**
     * 
     * @param v
     */
    private TipoBolloEnum(String v) {
        value = v;
    }

    /**
     * @return the value
     */
    public String value() {
        return value;
    }
}
