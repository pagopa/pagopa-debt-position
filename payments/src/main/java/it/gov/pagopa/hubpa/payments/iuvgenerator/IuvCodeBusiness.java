package it.gov.pagopa.hubpa.payments.iuvgenerator;

import it.gov.pagopa.hubpa.payments.iuvgenerator.validation.IuvCodeValidation;
import it.gov.pagopa.hubpa.payments.iuvgenerator.validation.IuvCodeValidationImpl;
import it.gov.pagopa.hubpa.payments.service.IuvCodeGenerator;

/**
 * Business logic class
 */
public class IuvCodeBusiness {

    private IuvCodeBusiness() {
	throw new IllegalStateException("Utility class");

    }

    /**
     * Generates the <code>iuv</code>
     * 
     * @param segregationCode
     * @return the <code>iuv</code>
     * @see pagopa.gov.it.toolkit.iuvGenerator.bean.IuvCodeGenerator
     * 
     */
    public static String generateIUV(Integer segregationCode, String nextValSequence) {
        IuvAlghoritmGenerator iuvGenerator = new IuvAlghoritmGenerator.Builder().build();
        return iuvGenerator.generate(segregationCode, nextValSequence);
    }

    /**
     * Validates a iuvCodeGenerator
     * 
     * @param iuvCodeGenerator
     *            the bean to validate.
     * @see IuvCodeGenerator
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.validation.DebtPositionValidationImpl
     */
    public static void validate(IuvCodeGenerator iuvCodeGenerator) {
        IuvCodeValidation iuvCodeValidation = new IuvCodeValidationImpl();
        iuvCodeValidation.validate(iuvCodeGenerator);
    }
}
