package it.gov.pagopa.hubpa.payments.generate.common.bean;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import it.gov.pagopa.hubpa.payments.generate.common.Constants;
import it.gov.pagopa.hubpa.payments.iuvgenerator.common.ErrorMessages;
import it.gov.pagopa.hubpa.payments.generate.rpt.enumeration.TipoBolloEnum;

/**
 * Component of a Single Payment of a Debt Position or a RPT containing the
 * "Marca da Bollo Digitale" data.
 * 
 * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail
 * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
 * @see pagopa.gov.it.toolkit.rptGenerator.validation.RptValidationImpl
 */
public class DatiMarcaBolloDigitale {

    /**
     * Initialization of <code>DatiMarcaBolloDigitale</code> Bean class
     */
    public static class Builder {

        private TipoBolloEnum tipoBollo;
        private String hashDocumento;
        private String provinciaResidenza;

        /**
         * Build a new <code>DatiMarcaBolloDigitale</code> Bean
         * 
         * @return a new instance of <code>DatiMarcaBolloDigitale</code>
         */
        public DatiMarcaBolloDigitale build() {
            return new DatiMarcaBolloDigitale(this);
        }

        /**
         * Set the tipoBollo
         * 
         * @param tipoBollo
         *            type of "Bollo" based on its enumeration.<br/>
         *            Not null.<br/>
         *            Enumeration:
         *            <ul>
         *            <li>IMPOSTA_DI_BOLLO - "01"
         *            </ul>
         * @return the <code>tipoBollo</code> field is set in
         *         <code>DatiMarcaBolloDigitale</code> builder
         */
        public Builder setTipoBollo(TipoBolloEnum tipoBollo) {
            this.tipoBollo = tipoBollo;
            return this;
        }

        /**
         * Set the hashDocumento
         * 
         * @param hashDocumento
         *            Digest of the document related to
         *            "Marca da Bollo Digitale". The hash algorithm to use is
         *            SHA-256. The 256-bit string (32 octets) result of that
         *            algorithm must be converted to base64.<br/>
         *            Not null nor empty.<br/>
         *            Max 70 chars.
         * @return the <code>hashDocumento</code> field is set in
         *         <code>DatiMarcaBolloDigitale</code> builder
         */
        public Builder setHashDocumento(String hashDocumento) {
            this.hashDocumento = hashDocumento;
            return this;
        }

        /**
         * Set the provinciaResidenza
         * 
         * @param provinciaResidenza
         *            province of residence of payer.<br/>
         *            Not null nor empty.<br/>
         *            Max 2 chars.<br/>
         *            Must respect the following regExp: "[A-Z]{2,2}"
         * @return the <code>provinciaResidenza</code> field is set in
         *         <code>DatiMarcaBolloDigitale</code> builder
         */
        public Builder setProvinciaResidenza(String provinciaResidenza) {
            this.provinciaResidenza = provinciaResidenza;
            return this;
        }
    }

    @NotNull
    private TipoBolloEnum tipoBollo;

    @NotEmpty
    @Size(max = 70)
    private String hashDocumento;

    @NotEmpty
    @Size(max = 2)
    @Pattern(regexp = Constants.REGEX_BOLLO_PROVINCIA, message = ErrorMessages.VALIDATION_INVALID_PROVINCIA)
    private String provinciaResidenza;

    /**
     * Private constructor
     */
    private DatiMarcaBolloDigitale() {
        // NOPE
    }

    /**
     * Private constructor
     * 
     * @param builder
     *            builder for instance generation
     */
    private DatiMarcaBolloDigitale(Builder builder) {
        this.tipoBollo = builder.tipoBollo;
        this.hashDocumento = builder.hashDocumento;
        this.provinciaResidenza = builder.provinciaResidenza;
    }

    /**
     * Get the tipoBollo
     * 
     * @return tipoBollo
     * @see pagopa.gov.it.toolkit.common.bean.DatiMarcaBolloDigitale.Builder#tipoBollo
     */
    public TipoBolloEnum getTipoBollo() {
        return tipoBollo;
    }

    /**
     * Get the hashDocumento
     * 
     * @return hashDocumento
     * @see pagopa.gov.it.toolkit.common.bean.DatiMarcaBolloDigitale.Builder#hashDocumento
     */
    public String getHashDocumento() {
        return hashDocumento;
    }

    /**
     * Get the provinciaResidenza
     * 
     * @return provinciaResidenza
     * @see pagopa.gov.it.toolkit.common.bean.DatiMarcaBolloDigitale.Builder#provinciaResidenza
     */
    public String getProvinciaResidenza() {
        return provinciaResidenza;
    }
}
