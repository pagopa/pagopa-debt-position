package it.gov.pagopa.payments.iuvgenerator.validation;

import it.gov.pagopa.payments.iuvgenerator.exception.ValidationException;
import it.gov.pagopa.payments.service.IuvCodeGenerator;

/**
 * Validation of IUV interface
 */
public interface IuvCodeValidation {

    /**
     * Validate the iuvCodeGenerator
     * 
     * @param iuvCodeGenerator
     *            the IUV code generator
     * @throws ValidationException
     */
    void validate(IuvCodeGenerator iuvCodeGenerator) throws ValidationException;

    /**
     * Constraints validation by annotation of the single input of the
     * iuvCodeGenerator
     * 
     * @param objectToValidate
     * @throws ValidationException
     */
    <T> void checkConstraints(T objectToValidate) throws ValidationException;
}
