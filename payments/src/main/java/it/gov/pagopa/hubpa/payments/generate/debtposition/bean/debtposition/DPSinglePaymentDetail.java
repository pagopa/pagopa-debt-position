package it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition;

import java.math.BigDecimal;

import javax.validation.Valid;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Digits;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import it.gov.pagopa.hubpa.payments.generate.common.Constants;
import it.gov.pagopa.hubpa.payments.iuvgenerator.common.ErrorMessages;
import it.gov.pagopa.hubpa.payments.generate.common.bean.DatiMarcaBolloDigitale;

/**
 * Component of a Debt Position containing the details of the individual
 * payments.
 * 
 * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.DebtPosition
 * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
 * @see pagopa.gov.it.toolkit.debtPositionGenerator.validation.DebtPositionValidationImpl
 */
public class DPSinglePaymentDetail {

    /**
     * Initialization of <code>DPSinglePaymentDetail</code> Bean class
     */
    public static class Builder {

        private BigDecimal amountSinglePayment;
        private String causalDescriptionSinglePayment;
        private Integer orderSinglePayment;
        private String creditIban;
        private String creditBic;
        private String supportIban;
        private String supportBic;
        private DatiMarcaBolloDigitale datiMarcaBolloDigitale;

        /**
         * Build a new <code>DPSinglePaymentDetail</code> Bean
         * 
         * @return a new instance of <code>DPSinglePaymentDetail</code>
         */
        public DPSinglePaymentDetail build() {
            return new DPSinglePaymentDetail(this);
        }

        /**
         * Set the amountSinglePayment
         * 
         * @param amountSinglePayment
         *            the amount to be paid.<br/>
         *            Not null.<br/>
         *            Max 12 digits, 2 decimals.<br/>
         *            Min value = 0.01
         * @return the <code>amountSinglePayment</code> field is set in
         *         <code>DPSinglePaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.validation.DebtPositionValidationImpl
         */
        public Builder setAmountSinglePayment(BigDecimal amountSinglePayment) {
            this.amountSinglePayment = amountSinglePayment;
            return this;
        }

        /**
         * Set the causalDescriptionSinglePayment
         * 
         * @param causalDescriptionSinglePayment
         *            the reason for the single payment.<br/>
         *            Not null nor empty.<br/>
         *            Max 140 chars.
         * @return the <code>causalDescriptionSinglePayment</code> field is set
         *         in <code>DPSinglePaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         */
        public Builder setCausalDescriptionSinglePayment(String causalDescriptionSinglePayment) {
            this.causalDescriptionSinglePayment = causalDescriptionSinglePayment;
            return this;
        }

        /**
         * Set the orderSinglePayment
         * 
         * @param orderSinglePayment
         *            single payment order in debtPosition<br/>
         *            Not null.<br/>
         *            Max 1 digit.
         * @return the <code>orderSinglePayment</code> field is set in
         *         <code>DPSinglePaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         */
        public Builder setOrderSinglePayment(Integer orderSinglePayment) {
            this.orderSinglePayment = orderSinglePayment;
            return this;
        }

        /**
         * Set the creditIban
         * 
         * @param creditIban
         *            iban of the account to be credited.<br/>
         *            Max 35 chars.<br/>
         *            Must respect the following regExp:
         *            "[a-zA-Z]{2,2}[0-9]{2,2}[a-zA-Z0-9]{1,30}"
         * @return the <code>creditIban</code> field is set in
         *         <code>DPSinglePaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         */
        public Builder setCreditIban(String creditIban) {
            this.creditIban = creditIban;
            return this;
        }

        /**
         * Set the creditBic
         * 
         * @param creditBic
         *            bic of the credit bank.<br/>
         *            Max 11 chars.
         * @return the <code>creditBic</code> field is set in
         *         <code>DPSinglePaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         */
        public Builder setCreditBic(String creditBic) {
            this.creditBic = creditBic;
            return this;
        }

        /**
         * Set the supportIban
         * 
         * @param supportIban
         *            iban of the account to be credited to a PSP that will
         *            transfer the funds collected on the indicated account in
         *            the <code>creditIban</code>.<br/>
         *            Max 35 chars.
         * @return the <code>supportIban</code> field is set in
         *         <code>DPSinglePaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         */
        public Builder setSupportIban(String supportIban) {
            this.supportIban = supportIban;
            return this;
        }

        /**
         * Set the supportBic
         * 
         * @param supportBic
         *            bic of the support bank.<br/>
         *            Max 11 chars.
         * @return the <code>supportBic</code> field is set in
         *         <code>DPSinglePaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         */
        public Builder setSupportBic(String supportBic) {
            this.supportBic = supportBic;
            return this;
        }

        /**
         * Set the datiMarcaBolloDigitale
         * 
         * @param datiMarcaBolloDigitale
         *            "Marca da Bollo Digitale" data
         * @return the <code>datiMarcaBolloDigitale</code> field is set in
         *         <code>DPSinglePaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.common.bean.DatiMarcaBolloDigitale
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         */
        public Builder setDatiMarcaBolloDigitale(DatiMarcaBolloDigitale datiMarcaBolloDigitale) {
            this.datiMarcaBolloDigitale = datiMarcaBolloDigitale;
            return this;
        }
    }

    @NotNull
    @Digits(integer = 10, fraction = 2)
    @DecimalMin(value = "0.01", message = ErrorMessages.VALIDATION_AMOUNT_MIN)
    private BigDecimal amountSinglePayment;

    @NotEmpty
    @Size(max = 140)
    private String causalDescriptionSinglePayment;

    @NotNull
    @Digits(integer = 1, fraction = 0)
    private Integer orderSinglePayment;

    @Size(max = 35)
    @Pattern(regexp = Constants.REGEX_IBAN, message = ErrorMessages.VALIDATION_INVALID_IBAN)
    private String creditIban;

    @Size(max = 11)
    @Pattern(regexp = Constants.REGEX_BIC, message = ErrorMessages.VALIDATION_INVALID_BIC)
    private String creditBic;

    @Size(max = 35)
    @Pattern(regexp = Constants.REGEX_IBAN, message = ErrorMessages.VALIDATION_INVALID_IBAN)
    private String supportIban;

    @Size(max = 11)
    @Pattern(regexp = Constants.REGEX_BIC, message = ErrorMessages.VALIDATION_INVALID_BIC)
    private String supportBic;

    @Valid
    private DatiMarcaBolloDigitale datiMarcaBolloDigitale;

    /**
     * Private constructor
     */
    private DPSinglePaymentDetail() {
        // NOPE
    }

    /**
     * Private constructor
     * 
     * @param builder
     *            builder for instance generation
     */
    private DPSinglePaymentDetail(Builder builder) {
        this.amountSinglePayment = builder.amountSinglePayment;
        this.causalDescriptionSinglePayment = builder.causalDescriptionSinglePayment;
        this.orderSinglePayment = builder.orderSinglePayment;
        this.creditIban = builder.creditIban;
        this.creditBic = builder.creditBic;
        this.supportIban = builder.supportIban;
        this.supportBic = builder.supportBic;
        this.datiMarcaBolloDigitale = builder.datiMarcaBolloDigitale;
    }

    /**
     * Get the amountSinglePayment
     * 
     * @return the amountSinglePayment
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail.Builder#amountSinglePayment
     */
    public BigDecimal getAmountSinglePayment() {
        return amountSinglePayment;
    }

    /**
     * Get the causalDescriptionSinglePayment
     * 
     * @return the causalDescriptionSinglePayment
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail.Builder#causalDescriptionSinglePayment
     */
    public String getCausalDescriptionSinglePayment() {
        return causalDescriptionSinglePayment;
    }

    /**
     * Get the orderSinglePayment
     * 
     * @return the orderSinglePayment
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail.Builder#orderSinglePayment
     */
    public Integer getOrderSinglePayment() {
        return orderSinglePayment;
    }

    /**
     * Get the creditIban
     * 
     * @return the creditIban
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail.Builder#creditIban
     */
    public String getCreditIban() {
        return creditIban;
    }

    /**
     * Get the creditBic
     * 
     * @return the creditBic
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail.Builder#creditBic
     */
    public String getCreditBic() {
        return creditBic;
    }

    /**
     * Get the supportIban
     * 
     * @return the supportIban
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail.Builder#supportIban
     */
    public String getSupportIban() {
        return supportIban;
    }

    /**
     * Get the supportBic
     * 
     * @return the supportBic
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail.Builder#supportBic
     */
    public String getSupportBic() {
        return supportBic;
    }

    /**
     * Get the datiMarcaBolloDigitale
     * 
     * @return the datiMarcaBolloDigitale
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail.Builder#datiMarcaBolloDigitale
     */
    public DatiMarcaBolloDigitale getDatiMarcaBolloDigitale() {
        return datiMarcaBolloDigitale;
    }
}
