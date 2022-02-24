package it.gov.pagopa.hubpa.payments.iuvgenerator.common;

/**
 * All the Exception error messages
 */
public class ErrorMessages {

    private ErrorMessages() {
	throw new IllegalStateException("Utility class");

    }
    
    public static final String VALIDATION_APPLICATION_CODE_ERROR = "ApplicationCode cannot be null for AuxDigit 0";
    public static final String UNEXPECTED_GENERATED_VALUE_ERROR = "Unexpected generated value: ";
    public static final String VALIDATION_ALGORITHM_NOT_IMPLEMENTED_ERROR = "Algorithm not implemented for AuxDigit: ";
    public static final String VALIDATION_AMOUNT_MIN = "The value must be grater than zero";
    public static final String VALIDATION_AMOUNTS_ERROR = "The sum of the single amounts does not coincide with the total amount of the payment";
    public static final String VALIDATION_AUXDIGIT_ERROR = "AuxDigit value not valid";
    public static final String VALIDATION_AUXDIGIT_NOT_ALLOWED_ERROR = "AuxDigit not allowed: ";
    public static final String VALIDATION_CHECK_DIGIT_ERROR = "Invalid iuv check digit";
    public static final String VALIDATION_ERROR = "Validation Errors: ";
    public static final String VALIDATION_FC_DIGIT_ERROR = "Invalid Digit: ";
    public static final String VALIDATION_FC_YEAR_ERROR = "Invalid Year: ";
    public static final String VALIDATION_FISCAL_CODE_ERROR = "Fiscal code is not formally correct";
    public static final String VALIDATION_IBAN_ADDEBITO_ERROR = "ibanAddebito mandatory if TipoVersamento [AD]";
    public static final String VALIDATION_INSTALLMENTS_MISSING_DOCUMENT_NUMBER = "Missing Document number: ";
    public static final String VALIDATION_INSTALLMENTS_MISSING_INSTALLMENT_NUMBER = "Missing Installment number: ";
    public static final String VALIDATION_INVALID_BIC = "Invalid BIC";
    public static final String VALIDATION_INVALID_DATI_SPECIFICI_RISCOSSIONE = "Invalid datiSpecificiRiscossione";
    public static final String VALIDATION_INVALID_DOCUMENT_NUMBER = "Document Number value unnecessary";
    public static final String VALIDATION_INVALID_EMAIL = "Invalid email";
    public static final String VALIDATION_INVALID_IBAN = "Invalid iban";
    public static final String VALIDATION_INVALID_IBAN_ACCREDITO_ERROR = "ibanAccredito not necessary if DatiMarcaBolloDigitale is present";
    public static final String VALIDATION_INVALID_INSTALLMENT_NUMBER = "Installment Number value unnecessary";
    public static final String VALIDATION_INVALID_PROVINCIA = "Invalid province";
    public static final String VALIDATION_MANDATORY_IBAN_ACCREDITO_ERROR = "ibanAccredito mandatory if DatiMarcaBolloDigitale is absent";
    public static final String VALIDATION_ONLY_1_INSTALLMENT = "DebtPosition with only one installment";
    public static final String VALIDATION_SEGREGATION_CODE_ERROR = "SegregationCode cannot be null for AuxDigit 3";
    public static final String VALIDATION_SINGLE_PAYMENT_LIST_SIZE_ERROR = "The size of Single Payment List has been exceeded";
    public static final String VALIDATION_UPPERCASE_FIELD = "The field must be uppercase";
    public static final String VALIDATION_VAT_NUMBER_ERROR = "VAT number is not formally correct";
}
