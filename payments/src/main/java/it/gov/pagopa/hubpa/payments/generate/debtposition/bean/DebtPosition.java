package it.gov.pagopa.hubpa.payments.generate.debtposition.bean;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPayer;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPSinglePaymentDetail;

/**
 * Main class of Debt Position Bean
 */
public class DebtPosition {

    /**
     * Initialization of <code>DebtPosition</code> Bean class
     */
    public static class Builder {

        private DPPayer payer;
        private DPPaymentDetail paymentDetail;
        private List<DPSinglePaymentDetail> singlePaymentDetailList;

        /**
         * Build a new <code>DebtPosition</code> Bean
         * 
         * @return a new instance of <code>DebtPosition</code>
         */
        public DebtPosition build() {
            return new DebtPosition(this);
        }

        /**
         * Set the payer
         * 
         * @param payer
         *            structure containing the payer data.<br/>
         *            Not null.
         * @return the <code>payer</code> field is set in
         *         <code>DebtPosition</code> builder
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPayer
         */
        public Builder setPayer(DPPayer payer) {
            this.payer = payer;
            return this;
        }

        /**
         * 
         * Set the paymentDetail
         * 
         * @param paymentDetail
         *            structure containing the payment details.<br/>
         *            Not null.
         * @return the <code>paymentDetail</code> field is set in
         *         <code>DebtPosition</code> builder
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail
         */
        public Builder setPaymentDetail(DPPaymentDetail paymentDetail) {
            this.paymentDetail = paymentDetail;
            return this;
        }

        /**
         * Set the singlePaymentDetailList
         * 
         * @param singlePaymentDetailList
         *            structure containing the details of the individual
         *            payments.<br/>
         *            Not null nor empty.
         * @return the <code>singlePaymentDetailList</code> field is set in
         *         <code>DebtPosition</code> builder
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPSinglePaymentDetail
         * @see pagopa.gov.it.toolkit.debtPositionGenerator.validation.DebtPositionValidationImpl
         */
        public Builder setSinglePaymentsDetail(List<DPSinglePaymentDetail> singlePaymentDetailList) {
            this.singlePaymentDetailList = singlePaymentDetailList;
            return this;
        }
    }

    @NotNull
    @Valid
    private DPPayer payer;

    @NotNull
    @Valid
    private DPPaymentDetail paymentDetail;

    @NotEmpty
    @Valid
    private List<DPSinglePaymentDetail> singlePaymentDetailList;

    /**
     * Private constructor
     */
    private DebtPosition() {
        // NOPE
    }

    /**
     * Private constructor
     * 
     * @param builder
     *            builder for instance generation
     */
    private DebtPosition(Builder builder) {
        this.payer = builder.payer;
        this.paymentDetail = builder.paymentDetail;
        this.singlePaymentDetailList = builder.singlePaymentDetailList;
    }

    /**
     * Get the payer
     * 
     * @return the payer
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.DebtPosition.Builder#payer
     */
    public DPPayer getPayer() {
        return payer;
    }

    /**
     * Get the paymentDetail
     * 
     * @return the paymentDetail
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.DebtPosition.Builder#paymentDetail
     */
    public DPPaymentDetail getPaymentDetail() {
        return paymentDetail;
    }

    /**
     * Get the singlePaymentDetailList
     * 
     * @return the singlePaymentDetailList
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.DebtPosition.Builder#singlePaymentDetailList
     */
    public List<DPSinglePaymentDetail> getSinglePaymentDetailList() {
        return singlePaymentDetailList;
    }
}
