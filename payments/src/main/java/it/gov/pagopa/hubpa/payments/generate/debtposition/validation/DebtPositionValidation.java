package it.gov.pagopa.hubpa.payments.generate.debtposition.validation;

import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.debtposition.exception.ValidationException;

/**
 * Validation of DebtPosition interface
 */
public interface DebtPositionValidation {

    /**
     * Validate the debtPosition
     * 
     * @param debtPosition
     *            the Debt Position Bean
     * @throws ValidationException
     */
    void validate(DebtPosition debtPosition) throws ValidationException;

    /**
     * Constraints validation by annotation of the debt position or one of its
     * components
     * 
     * @param objectToValidate
     * @throws ValidationException
     */
    <T> void checkConstraints(T objectToValidate) throws ValidationException;
}
