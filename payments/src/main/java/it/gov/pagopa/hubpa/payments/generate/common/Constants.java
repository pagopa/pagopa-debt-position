package it.gov.pagopa.hubpa.payments.generate.common;

/**
 * All the common constants and regExp
 */
public class Constants {

    private Constants() {
	throw new IllegalStateException("Utility class");
    }

    public static final int AUX_DIGIT_0 = 0;
    public static final int AUX_DIGIT_3 = 3;

    public static final int SINGLE_PAYMENT_LIST_MAX_SIZE = 5;

    public static final String UNIQUE_IDENTIFICATION_CODE_DEFAULT = "ANONIMO";
    public static final String UNIQUE_IDENTIFICATION_TYPE_DEFAULT = "F";
    public static final String TIPO_VERSAMENTO_DEFAULT = "CP";
    public static final String VERSIONE_OGGETTO_DEFAULT = "1.0";
    public static final String CREDENZIALI_PAGATORE_DEFAULT = null;

    public static final String REGEX_EMAIL = "[a-zA-Z0-9_\\.\\+\\-]+@[a-zA-Z0-9\\-]+(\\.[a-zA-Z0-9\\-]+)*";
    public static final String REGEX_IBAN = "[a-zA-Z]{2,2}[0-9]{2,2}[a-zA-Z0-9]{1,30}";
    public static final String REGEX_BIC = "[A-Z]{6,6}[A-Z2-9][A-NP-Z0-9]([A-Z0-9]{3,3}){0,1}";
    public static final String REGEX_DATI_SPECIFICI_RISCOSSIONE = "[0129]{1}/\\S{3,138}";
    public static final String REGEX_NATION = "[A-Z]{2,2}";
    public static final String REGEX_BOLLO_PROVINCIA = "[A-Z]{2,2}";
}
