package it.gov.pagopa.hubpa.payments.generate.debtposition.exception;

import java.util.Set;

import javax.validation.ConstraintViolation;

import it.gov.pagopa.hubpa.payments.iuvgenerator.common.ErrorMessages;

/**
 * Validation exception thrown in case of validation error
 */
public class ValidationException extends IllegalArgumentException {

    private static final long serialVersionUID = 6743696961083790897L;

    /**
     * 
     */
    public <T> ValidationException(Set<ConstraintViolation<T>> validationInputResults) {
        super(generateErrorMessage(validationInputResults));
    }

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

    /**
     * Validation by annotation
     * 
     * @param validationInputResults
     *            List of validation errors
     * @return a string containing the list of validation errors
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.validation.DebtPositionValidationImpl
     */
    private static <T> String generateErrorMessage(Set<ConstraintViolation<T>> validationInputResults) {
        String errorMsg = ErrorMessages.VALIDATION_ERROR;

        for (ConstraintViolation<T> validationError : validationInputResults) {
            String validationErrorMsg = validationError.getPropertyPath() + "[" + validationError.getMessage() + "]; ";
            errorMsg = errorMsg + validationErrorMsg;
        }

        return errorMsg;
    }
}
