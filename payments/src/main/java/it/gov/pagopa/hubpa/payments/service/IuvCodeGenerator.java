package it.gov.pagopa.hubpa.payments.service;

import javax.validation.constraints.Digits;

/**
 * Main class of IUV Code generator Bean
 */
public class IuvCodeGenerator {

    /**
     * Initialization of <code>IuvCodeGenerator</code> Bean class
     */
    public static class Builder {

        private int auxDigit;
        private Integer segregationCode;

        /**
         * Build a new <code>IuvCodeGenerator</code> Bean
         * 
         * @return a new instance of <code>IuvCodeGenerator</code>
         */
        public IuvCodeGenerator build() {
            return new IuvCodeGenerator(this);
        }

        /**
         * Set the auxDigit
         * 
         * @param auxDigit
         *            numeric value that defines the structure of the IUV code
         *            as a function of number of points of generation of the
         *            same.<br/>
         *            Not null.<br/>
         *            Max 1 digit.
         * @return the <code>auxDigit</code> field is set in
         *         <code>IuvCodeGenerator</code> builder
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail
         * @see pagopa.gov.it.toolkit.iuvGenerator.validation.IuvCodeValidationImpl
         */
        public Builder setAuxDigit(int auxDigit) {
            this.auxDigit = auxDigit;
            return this;
        }

        /**
         * Set the segregationCode
         * 
         * @param segregationCode
         *            numerical value associated with each point of generation
         *            of the IUV code.<br/>
         *            It is used to segregate the payment management domains of
         *            the institution.<br/>
         *            Used only if <code>auxDigit</code> = 3.<br/>
         *            Max 2 digits.
         * @return the <code>segregationCode</code> field is set in
         *         <code>IuvCodeGenerator</code> builder
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail
         * @see pagopa.gov.it.toolkit.iuvGenerator.validation.IuvCodeValidationImpl
         */
        public Builder setSegregationCode(Integer segregationCode) {
            this.segregationCode = segregationCode;
            return this;
        }
    }

    @Digits(integer = 1, fraction = 0)
    private int auxDigit;

    @Digits(integer = 2, fraction = 0)
    private Integer segregationCode;

    /**
     * Private constructor
     */
    private IuvCodeGenerator() {
        // NOPE
    }

    /**
     * Private constructor
     * 
     * @param builder
     *            builder for instance generation
     */
    private IuvCodeGenerator(Builder builder) {
        this.auxDigit = builder.auxDigit;
        this.segregationCode = builder.segregationCode;
    }

    /**
     * Get the auxDigit
     * 
     * @return the auxDigit
     * @see pagopa.gov.it.toolkit.iuvGenerator.bean.IuvCodeGenerator.Builder#auxDigit
     */
    public int getAuxDigit() {
        return auxDigit;
    }

    /**
     * Get the segregationCode
     * 
     * @return the segregationCode
     * @see pagopa.gov.it.toolkit.iuvGenerator.bean.IuvCodeGenerator.Builder#segregationCode
     */
    public Integer getSegregationCode() {
        return segregationCode;
    }

}
