package it.gov.pagopa.hubpa.payments.iuvgenerator.validation;

import java.util.Optional;
import java.util.Set;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;

import it.gov.pagopa.hubpa.payments.iuvgenerator.common.ErrorMessages;
import it.gov.pagopa.hubpa.payments.iuvgenerator.exception.ValidationException;
import it.gov.pagopa.hubpa.payments.service.IuvCodeGenerator;

/**
 * Implementation of IuvCodeValidation interface
 */
public class IuvCodeValidationImpl implements IuvCodeValidation {

    /**
     * Validate the debtPosition<br/>
     * The validation includes:
     * <ul>
     * <li>checkConstraints - validation by annotation
     * <li>checkAuxDigit3 - if <code>auxDigit</code> = 3
     * <code>segregationCode</code> must be present
     * </ul>
     * 
     * @param iuvCodeGenerator
     *            the bean to validate
     * @throws ValidationException
     * @see IuvCodeGenerator
     * @see ValidationException
     */
    @Override
    public void validate(IuvCodeGenerator iuvCodeGenerator) throws ValidationException {
        checkConstraints(iuvCodeGenerator);

        checkAuxDigit3(iuvCodeGenerator);
    }

    /**
     * @param objectToValidate
     *            the bean to validate.
     * @throws ValidationException
     * @see ValidationException
     */
    public <T> void checkConstraints(T objectToValidate) throws ValidationException {
        Validator validator = Validation.buildDefaultValidatorFactory().getValidator();
        Set<ConstraintViolation<T>> validationInputResults = validator.validate(objectToValidate);
        if (!validationInputResults.isEmpty()) {
            throw new ValidationException(validationInputResults);
        }
    }

    /**
     * @param iuvCodeGenerator
     * @see IuvCodeGenerator
     */
    private void checkAuxDigit3(IuvCodeGenerator iuvCodeGenerator) {
        if (iuvCodeGenerator.getAuxDigit() == 3 && 
        	Optional.ofNullable(iuvCodeGenerator.getSegregationCode())
        	.orElse(0) == 0) {
                throw new ValidationException(ErrorMessages.VALIDATION_SEGREGATION_CODE_ERROR);
            }
        
    }
}
