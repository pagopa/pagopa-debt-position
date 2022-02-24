package it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

/**
 * Component of a Payment Notice containing the Creditor Institution data.
 * 
 * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PaymentNotice
 */
public class PNCreditorInstitution {

    /**
     * Initialization of <code>PNCreditorInstitution</code> Bean class
     */
    public static class Builder {

        private byte[] logo;
        private String name;
        private String sector;
        private String info;
        private String fiscalCode;
        private String cbillCode;
        private String postalAccountHolder;
        private String postalAccountNumber;
        private String postalAuthorizationCode;
        private String website;

        /**
         * Build a new <code>PNCreditorInstitution</code> Bean
         * 
         * @return a new instance of <code>PNCreditorInstitution</code>
         */
        public PNCreditorInstitution build() {
            return new PNCreditorInstitution(this);
        }

        /**
         * Set the logo
         * 
         * @param logo
         *            the byte array of logo.<br/>
         *            Not null nor empty.
         * @return the <code>logo</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setLogo(byte[] logo) {
            this.logo = logo;
            return this;
        }

        /**
         * Set the name
         * 
         * @param name
         *            a list of DebtPosition.<br/>
         *            If <code>debtPositionList</code> size > 1 then there are
         *            installments.<br/>
         *            Not null nor empty.<br/>
         *            Max size 50 chars.
         * @return the <code>name</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setName(String name) {
            this.name = name;
            return this;
        }

        /**
         * Set the sector
         * 
         * @param sector
         *            name of the unit organizational that manages the payment.
         *            <br/>
         *            Not null nor empty.<br/>
         *            Max size 50 chars.
         * @return the <code>sector</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setSector(String sector) {
            this.sector = sector;
            return this;
        }

        /**
         * Set the info
         * 
         * @param info
         *            references of information services o assistance of the
         *            Creditor Institution intended for the citizen.<br/>
         *            Not null nor empty.<br/>
         *            Max size 100 chars.
         * @return the <code>info</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setInfo(String info) {
            this.info = info;
            return this;
        }

        /**
         * Set the fiscalCode
         * 
         * @param fiscalCode
         *            fiscal code or VAT number of the Creditor Institution.
         *            <br/>
         *            Not null nor empty.<br/>
         *            Max size 16 chars.
         * @return the <code>fiscalCode</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setFiscalCode(String fiscalCode) {
            this.fiscalCode = fiscalCode;
            return this;
        }

        /**
         * Set the cbillCode
         * 
         * @param cbillCode
         *            interbank code of the Creditor Institution.<br/>
         *            Not null nor empty.<br/>
         *            Size exactly 5 chars.
         * @return the <code>cbillCode</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setCbillCode(String cbillCode) {
            this.cbillCode = cbillCode;
            return this;
        }

        /**
         * Set the postalAccountHolder
         * 
         * @param postalAccountHolder
         *            heading of the c/c postal service of the Creditor
         *            Institution.<br/>
         *            Max size 50 chars.
         * @return the <code>postalAccountHolder</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setPostalAccountHolder(String postalAccountHolder) {
            this.postalAccountHolder = postalAccountHolder;
            return this;
        }

        /**
         * Set the postalAccountNumber
         * 
         * @param postalAccountNumber
         *            number of the c/c postal service of the Creditor
         *            Institution.<br/>
         *            .<br/>
         *            Size exactly 12 chars.
         * @return the <code>postalAccountNumber</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setPostalAccountNumber(String postalAccountNumber) {
            this.postalAccountNumber = postalAccountNumber;
            return this;
        }

        /**
         * Set the postalAuthorizationCode
         * 
         * @param postalAuthorizationCode
         *            Authorization code of Poste Italiane to the self-printing.
         *            <br/>
         *            Max size 50 chars.
         * @return the <code>postalAuthorizationCode</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setPostalAuthorizationCode(String postalAuthorizationCode) {
            this.postalAuthorizationCode = postalAuthorizationCode;
            return this;
        }

        /**
         * Set the website
         * 
         * @param website
         *            website of the Creditor Institution.<br/>
         *            Not null nor empty.<br/>
         *            Max size 100 chars.
         * @return the <code>website</code> field is set in
         *         <code>PNCreditorInstitution</code> builder
         */
        public Builder setWebsite(String website) {
            this.website = website;
            return this;
        }
    }

    @NotEmpty
    private byte[] logo;

    @NotEmpty
    @Size(max = 50)
    private String name;

    @NotEmpty
    @Size(max = 50)
    private String sector;

    @NotEmpty
    @Size(max = 100)
    private String info;

    @NotEmpty
    @Size(max = 16)
    private String fiscalCode;

    @NotEmpty
    @Size(min = 5, max = 5)
    private String cbillCode;

    @Size(max = 50)
    private String postalAccountHolder;

    @Size(min = 12, max = 12)
    private String postalAccountNumber;

    @Size(max = 50)
    private String postalAuthorizationCode;

    @NotEmpty
    @Size(max = 100)
    private String website;

    /**
     * Private constructor
     */
    private PNCreditorInstitution() {
        // NOPE
    }

    /**
     * Private constructor
     * 
     * @param builder
     *            builder for instance generation
     */
    private PNCreditorInstitution(Builder builder) {
        this.logo = builder.logo;
        this.name = builder.name;
        this.sector = builder.sector;
        this.info = builder.info;
        this.fiscalCode = builder.fiscalCode;
        this.cbillCode = builder.cbillCode;
        this.postalAccountHolder = builder.postalAccountHolder;
        this.postalAccountNumber = builder.postalAccountNumber;
        this.postalAuthorizationCode = builder.postalAuthorizationCode;
        this.website = builder.website;
    }

    /**
     * Get the logo
     * 
     * @return the logo
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#logo
     */
    public byte[] getLogo() {
        return logo;
    }

    /**
     * Get the name
     * 
     * @return the name
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#name
     */
    public String getName() {
        return name;
    }

    /**
     * Get the sector
     * 
     * @return the sector
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#sector
     */
    public String getSector() {
        return sector;
    }

    /**
     * Get the info
     * 
     * @return the info
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#info
     */
    public String getInfo() {
        return info;
    }

    /**
     * Get the fiscalCode
     * 
     * @return the fiscalCode
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#fiscalCode
     */
    public String getFiscalCode() {
        return fiscalCode.toUpperCase();
    }

    /**
     * Get the cbillCode
     * 
     * @return the cbillCode
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#cbillCode
     */
    public String getCbillCode() {
        return cbillCode;
    }

    /**
     * Get the postalAccountHolder
     * 
     * @return the postalAccountHolder
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#postalAccountHolder
     */
    public String getPostalAccountHolder() {
        return postalAccountHolder;
    }

    /**
     * Get the postalAccountNumber
     * 
     * @return the postalAccountNumber
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#postalAccountNumber
     */
    public String getPostalAccountNumber() {
        return postalAccountNumber;
    }

    /**
     * Get the postalAuthorizationCode
     * 
     * @return the postalAuthorizationCode
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#postalAuthorizationCode
     */
    public String getPostalAuthorizationCode() {
        return postalAuthorizationCode;
    }

    /**
     * Get the website
     * 
     * @return the website
     * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.bean.PNCreditorInstitution.Builder#website
     */
    public String getWebsite() {
        return website;
    }
}
