package it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition;

import java.math.BigDecimal;
import java.util.Date;

import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Digits;
import javax.validation.constraints.Future;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import it.gov.pagopa.hubpa.payments.generate.common.Constants;
import it.gov.pagopa.hubpa.payments.iuvgenerator.common.ErrorMessages;
import it.gov.pagopa.hubpa.payments.generate.debtposition.enumeration.PaymentStatusEnum;

/**
 * Component of a Debt Position containing the payment details.
 * 
 * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.DebtPosition
 */
public class DPPaymentDetail {

    /**
     * Initialization of <code>DPPaymentDetail</code> Bean class
     */
    public static class Builder {

        private String domainIdentifier;
        private int auxDigit;
        private Integer applicationCode;
        private Integer segregationCode;
        private String iuv;
        private String idTenant;
        private BigDecimal totalAmountPayment;
        private String causal;
        private Date expirationDate;
        private String specificCollectionData;
        private String documentNumber;
        private Integer installmentNumber;
        private String debitIban;
        private String debitBic;

        /**
         * Build a new <code>DPPaymentDetail</code> Bean
         * 
         * @return a new instance of <code>DPPaymentDetail</code>
         */
        public DPPaymentDetail build() {
            return new DPPaymentDetail(this);
        }

        /**
         * Set the domainIdentifier
         * 
         * @param domainIdentifier
         *            fiscal code of the structure that send the payment
         *            request.<br/>
         *            Not null nor empty.<br/>
         *            Max 16 chars.
         * @return the <code>domainIdentifier</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDominio
         */
        public Builder setDomainIdentifier(String domainIdentifier) {
            this.domainIdentifier = domainIdentifier;
            return this;
        }

        /**
         * Set the auxDigit
         * 
         * @param auxDigit
         *            numeric value that defines the structure of the IUV code
         *            as a function of number of points of generation of the
         *            same.<br/>
         *            Max 1 digit.
         * @return the <code>auxDigit</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.iuvGenerator.bean.IuvCodeGenerator
         */
        public Builder setAuxDigit(int auxDigit) {
            this.auxDigit = auxDigit;
            return this;
        }

        /**
         * Set the applicationCode
         * 
         * @param applicationCode
         *            numerical value used to identify the portion of the
         *            archive of pending payments affected by the transaction.
         *            <br/>
         *            Used only if <code>auxDigit</code> = 0.<br/>
         *            Max 2 digits.
         * @return the <code>applicationCode</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.iuvGenerator.bean.IuvCodeGenerator
         */
        public Builder setApplicationCode(Integer applicationCode) {
            this.applicationCode = applicationCode;
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
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.iuvGenerator.bean.IuvCodeGenerator
         */
        public Builder setSegregationCode(Integer segregationCode) {
            this.segregationCode = segregationCode;
            return this;
        }

        /**
         * Set the iuv
         * 
         * @param iuv
         *            unique reference assigned to the payment by the Creditor.
         *            <br/>
         *            if absent it is generated automatically.<br/>
         *            Max 35 chars.
         * @return the <code>iuv</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.iuvGenerator.IuvCodeGeneration
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiVersamento
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.validation.DebtPositionValidationImpl
         */
        public Builder setIuv(String iuv) {
            this.iuv = iuv;
            return this;
        }

        /**
         * Set the idTenant
         * <p>
         * Useful if the institution relies on an intermediary
         * 
         * @param idTenant
         *            unique identifier Internal payment of the institution.
         *            <br/>
         *            Max 50 chars.
         * @return the <code>idTenant</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.RptContainer
         */
        public Builder setIdTenant(String idTenant) {
            this.idTenant = idTenant;
            return this;
        }

        /**
         * Set the totalAmountPayment
         * <p>
         * Is the total sum of the amounts of the individual payments
         * 
         * @param totalAmountPayment
         *            the amount to be paid.<br/>
         *            Not null.<br/>
         *            Max 12 digits, 2 decimals.<br/>
         *            Min value = 0.01
         * @return the <code>totalAmountPayment</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiVersamento
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.validation.DebtPositionValidationImpl
         */
        public Builder setTotalAmountPayment(BigDecimal totalAmountPayment) {
            this.totalAmountPayment = totalAmountPayment;
            return this;
        }

        /**
         * Set the causal
         * 
         * @param causal
         *            the reason for the payment.<br/>
         *            Not null nor empty.<br/>
         *            Max 60 chars.
         * @return the <code>causal</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         */
        public Builder setCausal(String causal) {
            this.causal = causal;
            return this;
        }

        /**
         * Set the expirationDate
         * 
         * @param expirationDate
         *            payment due date.<br/>
         *            Only future date.
         * @return the <code>expirationDate</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         */
        public Builder setExpirationDate(Date expirationDate) {
            this.expirationDate = expirationDate;
            return this;
        }

        /**
         * Set the specificCollectionData
         * 
         * @param specificCollectionData
         *            the indication of the attribution of the specification
         *            entry.<br/>
         *            Max 140 chars.<br/>
         *            Must have this format: "tipoContabilità/codiceContabilità"
         *            which must respect the following regExp:
         *            "[0129]{1}/\S{3,138}"
         * @return the <code>uniqueIdentificationCode</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiSingoloVersamento
         */
        public Builder setSpecificCollectionData(String specificCollectionData) {
            this.specificCollectionData = specificCollectionData;
            return this;
        }

        /**
         * Set the documentNumber
         * 
         * @param documentNumber
         *            numeric code that aggregates multiple iuv codes and
         *            identifies all the installments of a payment.<br/>
         *            Max 35 chars.
         * @return the <code>documentNumber</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.validation.PaymentNoticeValidationImpl
         */
        public Builder setDocumentNumber(String documentNumber) {
            this.documentNumber = documentNumber;
            return this;
        }

        /**
         * Set the installmentNumber
         * 
         * @param installmentNumber
         *            number of an installment.<br/>
         *            <code>installmentNumber</code> = 0 indicates a single
         *            installment (Rata Unica).<br/>
         *            <code>installmentNumber</code> from 1 to n indicates the
         *            number of the installment.<br/>
         *            Max 2 digits.
         * @return the <code>installmentNumber</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.paymentNoticeGenerator.validation.PaymentNoticeValidationImpl
         */
        public Builder setInstallmentNumber(Integer installmentNumber) {
            this.installmentNumber = installmentNumber;
            return this;
        }

        /**
         * Set the debitIban
         * 
         * @param debitIban
         *            iban of the account from charge.<br/>
         *            Not null nor empty.<br/>
         *            Max 35 chars.<br/>
         *            Must respect the following regExp:
         *            "[a-zA-Z]{2,2}[0-9]{2,2}[a-zA-Z0-9]{1,30}"
         * @return the <code>debitIban</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiVersamento
         */
        public Builder setDebitIban(String debitIban) {
            this.debitIban = debitIban;
            return this;
        }

        /**
         * Set the debitBic
         * 
         * @param debitBic
         *            bic of the debit bank.<br/>
         *            Max 11 chars.<br/>
         *            Must respect the following regExp:
         *            "[A-Z]{6,6}[A-Z2-9][A-NP-Z0-9]([A-Z0-9]{3,3}){0,1}"
         * @return the <code>debitBic</code> field is set in
         *         <code>DPPaymentDetail</code> builder
         * @see pagopa.gov.it.toolkit.rptGenerator.bean.rpt.RptDatiVersamento
         */
        public Builder setDebitBic(String debitBic) {
            this.debitBic = debitBic;
            return this;
        }
    }

    @NotEmpty
    @Size(max = 16)
    private String domainIdentifier;

    @Digits(integer = 1, fraction = 0)
    private int auxDigit;

    @Digits(integer = 2, fraction = 0)
    private Integer applicationCode;

    @Digits(integer = 2, fraction = 0)
    private Integer segregationCode;

    @Size(max = 35)
    private String iuv;

    @Size(max = 50)
    private String idTenant;

    @NotNull
    @Digits(integer = 10, fraction = 2)
    @DecimalMin(value = "0.01", message = ErrorMessages.VALIDATION_AMOUNT_MIN)
    private BigDecimal totalAmountPayment;

    @NotEmpty
    @Size(max = 60)
    private String causal;

    @Future
    private Date expirationDate;

    @Size(max = 140)
    @Pattern(regexp = Constants.REGEX_DATI_SPECIFICI_RISCOSSIONE, message = ErrorMessages.VALIDATION_INVALID_DATI_SPECIFICI_RISCOSSIONE)
    private String specificCollectionData;

    @Size(max = 35)
    private String documentNumber;

    @Digits(integer = 2, fraction = 0)
    private Integer installmentNumber;

    private Date creationDate;
    private String noticeNumber;
    private PaymentStatusEnum paymentStatus;

    @NotEmpty
    @Size(max = 35)
    @Pattern(regexp = Constants.REGEX_IBAN, message = ErrorMessages.VALIDATION_INVALID_IBAN)
    private String debitIban;

    @Size(max = 11)
    @Pattern(regexp = Constants.REGEX_BIC, message = ErrorMessages.VALIDATION_INVALID_BIC)
    private String debitBic;

    /**
     * Private constructor
     */
    private DPPaymentDetail() {
        // NOPE
    }

    /**
     * Private constructor
     * 
     * @param builder
     *            builder for instance generation
     */
    private DPPaymentDetail(Builder builder) {
        this.domainIdentifier = builder.domainIdentifier;
        this.auxDigit = builder.auxDigit;
        this.applicationCode = builder.applicationCode;
        this.segregationCode = builder.segregationCode;
        this.iuv = builder.iuv;
        this.idTenant = builder.idTenant;
        this.totalAmountPayment = builder.totalAmountPayment;
        this.causal = builder.causal;
        this.expirationDate = builder.expirationDate;
        this.specificCollectionData = builder.specificCollectionData;
        this.documentNumber = builder.documentNumber;
        this.installmentNumber = builder.installmentNumber;
        this.creationDate = new Date();
        this.paymentStatus = PaymentStatusEnum.PAYABLE;
        this.debitIban = builder.debitIban;
        this.debitBic = builder.debitBic;
    }

    /**
     * Get the domainIdentifier
     * 
     * @return the domainIdentifier
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#domainIdentifier
     */
    public String getDomainIdentifier() {
        return domainIdentifier;
    }

    /**
     * Get the auxDigit
     * 
     * @return the auxDigit
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#auxDigit
     */
    public int getAuxDigit() {
        return auxDigit;
    }

    /**
     * Get the applicationCode
     * 
     * @return the applicationCode
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#applicationCode
     */
    public Integer getApplicationCode() {
        return applicationCode;
    }

    /**
     * Get the segregationCode
     * 
     * @return the segregationCode
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#segregationCode
     */
    public Integer getSegregationCode() {
        return segregationCode;
    }

    /**
     * Get the iuv
     * 
     * @return the iuv
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#iuv
     */
    public String getIuv() {
        return iuv;
    }

    /**
     * Get the idTenant
     * 
     * @return the idTenant
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#idTenant
     */
    public String getIdTenant() {
        return idTenant;
    }

    /**
     * Get the totalAmountPayment
     * 
     * @return the totalAmountPayment
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#totalAmountPayment
     */
    public BigDecimal getTotalAmountPayment() {
        return totalAmountPayment;
    }

    /**
     * Get the causal
     * 
     * @return the causal
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#causal
     */
    public String getCausal() {
        return causal;
    }

    /**
     * Get the expirationDate
     * 
     * @return the expirationDate
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#expirationDate
     */
    public Date getExpirationDate() {
        return expirationDate;
    }

    /**
     * Get the specificCollectionData
     * 
     * @return the specificCollectionData
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#specificCollectionData
     */
    public String getSpecificCollectionData() {
        return specificCollectionData;
    }

    /**
     * Get the documentNumber
     * 
     * @return the documentNumber
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#documentNumber
     */
    public String getDocumentNumber() {
        return documentNumber;
    }

    /**
     * Get the installmentNumber
     * 
     * @return the installmentNumber
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#installmentNumber
     */
    public Integer getInstallmentNumber() {
        return installmentNumber;
    }

    /**
     * Get the creationDate
     * 
     * @return the creationDate of DebtPosition
     */
    public Date getCreationDate() {
        return creationDate;
    }

    /**
     * Get the noticeNumber
     * 
     * @return the noticeNumber
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#noticeNumber
     */
    public String getNoticeNumber() {
        return noticeNumber;
    }

    /**
     * Get the paymentStatus
     * 
     * @return the paymentStatus
     */
    public PaymentStatusEnum getPaymentStatus() {
        return paymentStatus;
    }

    /**
     * Get the debitIban
     * 
     * @return the debitIban
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#debitIban
     */
    public String getDebitIban() {
        return debitIban;
    }

    /**
     * Get the debitBic
     * 
     * @return the debitBic
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#debitBic
     */
    public String getDebitBic() {
        return debitBic;
    }

    /**
     * Set the iuv
     * 
     * @param iuv
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail.Builder#iuv
     */
    protected void setIuv(String iuv) {
        this.iuv = iuv;
    }

    /**
     * Set the noticeNumber
     * 
     * @param noticeNumber
     *            the number of the payment notice. Generated by the iuv.
     */
    protected void setNoticeNumber(String noticeNumber) {
        this.noticeNumber = noticeNumber;
    }

    /**
     * Set the paymentStatus
     * 
     * @param paymentStatus
     *            the status of DebtPosition based on its enumeration.<br/>
     *            Enumeration:
     *            <ul>
     *            <li>PAYABLE - 1
     *            <li>NOT_PAYABLE - 2
     *            <li>CANCELED - 3
     *            <li>PAID - 4
     *            </ul>
     */
    protected void setPaymentStatus(PaymentStatusEnum paymentStatus) {
        this.paymentStatus = paymentStatus;
    }

    public void setNoticeNumberManual(String noticeNumber) {
	this.noticeNumber = noticeNumber;
    }
}
