package it.gov.pagopa.hubpa.payments.iuvgenerator;

import it.gov.pagopa.hubpa.payments.iuvgenerator.exception.ValidationException;

/**
 * IUV code generator with IUV alghoritm interface
 */
public interface IuvAlghoritmGenerator {

    /**
     * Initialization of <code>IuvAlghoritmGenerator</code> class
     */
    public static class Builder {

        /**
         * Build the IuvAlghoritmGenerator based on <code>auxDigit</code>
         * 
         * @return a new instance of <code>IuvAlghoritmGenerator</code>
         */
        public IuvAlghoritmGenerator build() throws ValidationException {
            return new IuvAlghoritmAuxDigit3();
        }
    }

    /**
     * Generates the IUV Code
     * 
     * @param segregationCode
     *            the segregation code
     * @return the IUV Code
     */
    String generate(Integer segregationCode, String nextValSequence);
}