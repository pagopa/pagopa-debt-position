package it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import it.gov.pagopa.hubpa.payments.generate.common.Constants;
import it.gov.pagopa.hubpa.payments.iuvgenerator.common.ErrorMessages;
import it.gov.pagopa.hubpa.payments.generate.rpt.xsd.StTipoIdentificativoUnivocoPersFG;

/**
 * Component of a Debt Position containing the payer data.
 * 
 * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.DebtPosition
 */
public class DPPayer {

    /**
     * Initialization of <code>DPPayer</code> Bean class
     */
    public static class Builder {

        private String uniqueIdentificationCode;
        private StTipoIdentificativoUnivocoPersFG uniqueIdentificationType;
        private String registry;
        private String address;
        private String numberStreet;
        private String locality;
        private String province;
        private String postalCode;
        private String nation;
        private String email;
        private String mobile;

        /**
         * Build a new <code>DPPayer</code> Bean
         * 
         * @return a new instance of <code>DPPayer</code>
         */
        public DPPayer build() {
            return new DPPayer(this);
        }

        /**
         * Set the uniqueIdentificationCode
         * 
         * @param uniqueIdentificationCode
         *            fiscal code or VAT number of the payer.<br/>
         *            Not null nor empty.<br/>
         *            Max 35 chars.
         * @return the <code>uniqueIdentificationCode</code> field is set in
         *         <code>DPPayer</code> builder
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.validation.DebtPositionValidationImpl
         */
        public Builder setUniqueIdentificationCode(String uniqueIdentificationCode) {
            this.uniqueIdentificationCode = uniqueIdentificationCode;
            return this;
        }

        /**
         * Set the uniqueIdentificationType
         * 
         * @param uniqueIdentificationType
         *            type of Identification based on its enumeration and based
         *            on uniqueIdentificationCode.<br/>
         *            Not null.<br/>
         *            Enumeration:
         *            <ul>
         *            <li>F - Persona Fisica
         *            <li>G - Persona Giuridica
         *            </ul>
         * @return the <code>uniqueIdentificationType</code> field is set in
         *         <code>DPPayer</code> builder
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.validation.DebtPositionValidationImpl
         */
        public Builder setUniqueIdentificationType(StTipoIdentificativoUnivocoPersFG uniqueIdentificationType) {
            this.uniqueIdentificationType = uniqueIdentificationType;
            return this;
        }

        /**
         * Set the registry
         * 
         * @param registry
         *            name of the payer.<br/>
         *            Not null nor empty.<br/>
         *            Max 70 chars.
         * @return the <code>registry</code> field is set in
         *         <code>DPPayer</code> builder
         */
        public Builder setRegistry(String registry) {
            this.registry = registry;
            return this;
        }

        /**
         * Set the address
         * 
         * @param address
         *            the street of the payer's address.<br/>
         *            Max 70 chars.
         * @return the <code>address</code> field is set in <code>DPPayer</code>
         *         builder
         */
        public Builder setAddress(String address) {
            this.address = address;
            return this;
        }

        /**
         * Set the numberStreet
         * 
         * @param numberStreet
         *            the number street of the payer's address.<br/>
         *            Max 16 chars.
         * @return the <code>numberStreet</code> field is set in
         *         <code>DPPayer</code> builder
         */
        public Builder setNumberStreet(String numberStreet) {
            this.numberStreet = numberStreet;
            return this;
        }

        /**
         * Set the locality
         * 
         * @param locality
         *            the locality of the payer's address.<br/>
         *            Max 35 chars.
         * @return the <code>locality</code> field is set in
         *         <code>DPPayer</code> builder
         */
        public Builder setLocality(String locality) {
            this.locality = locality;
            return this;
        }

        /**
         * Set the province
         * 
         * @param province
         *            the province of the payer's address.<br/>
         *            Max 35 chars.
         * @return the <code>province</code> field is set in
         *         <code>DPPayer</code> builder
         */
        public Builder setProvince(String province) {
            this.province = province;
            return this;
        }

        /**
         * Set the postalCode
         * 
         * @param postalCode
         *            the postalCode of the payer's address.<br/>
         *            Max 16 chars.
         * @return the <code>postalCode</code> field is set in
         *         <code>DPPayer</code> builder
         */
        public Builder setPostalCode(String postalCode) {
            this.postalCode = postalCode;
            return this;
        }

        /**
         * Set the nation
         * 
         * @param nation
         *            the nation of the payer's address.<br/>
         *            Max 2 chars.<br/>
         *            Must respect the following regExp: "[A-Z]{2,2}"
         * @return the <code>nation</code> field is set in <code>DPPayer</code>
         *         builder
         */
        public Builder setNation(String nation) {
            this.nation = nation;
            return this;
        }

        /**
         * Set the email
         * 
         * @param email
         *            email address of the payer.<br/>
         *            Max 256 chars.<br/>
         *            Must respect the following regExp:
         *            "[a-zA-Z0-9_\.\+\-]+@[a-zA-Z0-9\-]+(\.[a-zA-Z0-9\-]+)*"
         * @return the <code>email</code> field is set in <code>DPPayer</code>
         *         builder
         */
        public Builder setEmail(String email) {
            this.email = email;
            return this;
        }

        /**
         * Set the mobile
         * 
         * @param mobile
         *            telephone number of the payer.<br/>
         *            Max 19 chars.
         * @return the <code>mobile</code> field is set in <code>DPPayer</code>
         *         builder
         */
        public Builder setMobile(String mobile) {
            this.mobile = mobile;
            return this;
        }
    }

    @NotEmpty
    @Size(max = 35)
    private String uniqueIdentificationCode;

    @NotNull
    private StTipoIdentificativoUnivocoPersFG uniqueIdentificationType;

    @NotEmpty
    @Size(max = 70)
    private String registry;

    @Size(max = 70)
    private String address;

    @Size(max = 16)
    private String numberStreet;

    @Size(max = 35)
    private String locality;

    @Size(max = 35)
    private String province;

    @Size(max = 16)
    private String postalCode;

    @Size(max = 2)
    @Pattern(regexp = Constants.REGEX_NATION, message = ErrorMessages.VALIDATION_UPPERCASE_FIELD)
    private String nation;

    @Size(max = 256)
    @Pattern(regexp = Constants.REGEX_EMAIL, message = ErrorMessages.VALIDATION_INVALID_EMAIL)
    private String email;

    @Size(max = 19)
    private String mobile;

    /**
     * Private constructor
     */
    private DPPayer() {
        // NOPE
    }

    /**
     * Private constructor
     * 
     * @param builder
     *            builder for instance generation
     */
    private DPPayer(Builder builder) {
        this.uniqueIdentificationCode = builder.uniqueIdentificationCode;
        this.uniqueIdentificationType = builder.uniqueIdentificationType;
        this.registry = builder.registry;
        this.address = builder.address;
        this.numberStreet = builder.numberStreet;
        this.locality = builder.locality;
        this.province = builder.province;
        this.postalCode = builder.postalCode;
        this.nation = builder.nation;
        this.email = builder.email;
        this.mobile = builder.mobile;
    }

    /**
     * Get the uniqueIdentificationCode
     * 
     * @return the uniqueIdentificationCode
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#uniqueIdentificationCode
     */
    public String getUniqueIdentificationCode() {
        return uniqueIdentificationCode;
    }

    /**
     * Get the uniqueIdentificationType
     * 
     * @return the uniqueIdentificationType
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#uniqueIdentificationType
     */
    public StTipoIdentificativoUnivocoPersFG getUniqueIdentificationType() {
        return uniqueIdentificationType;
    }

    /**
     * Get the registry
     * 
     * @return the registry
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#registry
     */
    public String getRegistry() {
        return registry;
    }

    /**
     * Get the address
     * 
     * @return the address
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#address
     */
    public String getAddress() {
        return address;
    }

    /**
     * Get the numberStreet
     * 
     * @return the numberStreet
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#numberStreet
     */
    public String getNumberStreet() {
        return numberStreet;
    }

    /**
     * Get the locality
     * 
     * @return the locality
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#locality
     */
    public String getLocality() {
        return locality;
    }

    /**
     * Get the province
     * 
     * @return the province
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#province
     */
    public String getProvince() {
        return province;
    }

    /**
     * Get the postalCode
     * 
     * @return the postalCode
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#postalCode
     */
    public String getPostalCode() {
        return postalCode;
    }

    /**
     * Get the nation
     * 
     * @return the nation
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#nation
     */
    public String getNation() {
        return nation;
    }

    /**
     * Get the email
     * 
     * @return the email
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#email
     */
    public String getEmail() {
        return email;
    }

    /**
     * Get the mobile
     * 
     * @return the mobile
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer.Builder#mobile
     */
    public String getMobile() {
        return mobile;
    }
}
