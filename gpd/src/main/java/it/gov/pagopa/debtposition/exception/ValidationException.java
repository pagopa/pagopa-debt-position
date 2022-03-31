package it.gov.pagopa.debtposition.exception;

/**
 * Validation exception thrown in case of validation error
 */
public class ValidationException extends IllegalArgumentException {

    private static final long serialVersionUID = 6743696961083790897L;


    /**
     *
     */
    public ValidationException() {
    }

    /**
     * @param s
     */
    public ValidationException(String s) {
        super(s);
    }

    /**
     * @param cause
     */
    public ValidationException(Throwable cause) {
        super(cause);
    }

    /**
     * @param message
     * @param cause
     */
    public ValidationException(String message, Throwable cause) {
        super(message, cause);
    }
}
