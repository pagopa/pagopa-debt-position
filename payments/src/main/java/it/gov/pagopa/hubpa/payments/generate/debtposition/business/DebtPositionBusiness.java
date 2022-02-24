package it.gov.pagopa.hubpa.payments.generate.debtposition.business;

import java.text.DecimalFormat;

import it.gov.pagopa.hubpa.payments.generate.common.Constants;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPUpdater;
import it.gov.pagopa.hubpa.payments.generate.debtposition.enumeration.PaymentStatusEnum;
import it.gov.pagopa.hubpa.payments.generate.debtposition.validation.DebtPositionValidation;
import it.gov.pagopa.hubpa.payments.generate.debtposition.validation.DebtPositionValidationImpl;

/**
 * Business logic class
 */
public class DebtPositionBusiness {
    private DebtPositionBusiness() {
	throw new IllegalStateException("DebtPositionBusiness class");
    }

    /**
     * Validates a debt position
     * 
     * @param debtPosition
     * @see DebtPosition
     */
    public static void validate(DebtPosition debtPosition) {
        DebtPositionValidation debtPositionValidation = new DebtPositionValidationImpl();
        debtPositionValidation.validate(debtPosition);
    }

    /**
     * Constraints validation by annotation of the debt position or one of its
     * components
     * 
     * @param objectToValidate
     *            the bean to validate. It can be the debt position or one of
     *            its components.
     * @see DebtPositionValidationImpl
     * @see DebtPosition
     */
    public static <T> void validateConstraints(T objectToValidate) {
        DebtPositionValidation debtPositionValidation = new DebtPositionValidationImpl();
        debtPositionValidation.checkConstraints(objectToValidate);
    }

    /**
     * Generates the <code>noticeNumber</code> by <code>iuv</code> and updates
     * the <code>debtPosition</code>
     * 
     * @param debtPosition
     * @see DebtPosition
     * @see pagopa.gov.it.toolkit.debtPositionGenerator.bean.debtPosition.DPPaymentDetail
     */
    public static void generateNoticeNumber(DebtPosition debtPosition) {
        int auxDigit = debtPosition.getPaymentDetail().getAuxDigit();
        if (auxDigit == Constants.AUX_DIGIT_0) {
            DPUpdater.setNoticeNumber(debtPosition.getPaymentDetail(),
                    String.valueOf(Constants.AUX_DIGIT_0)
                            + new DecimalFormat("00").format(debtPosition.getPaymentDetail().getApplicationCode())
                            + debtPosition.getPaymentDetail().getIuv());
        } else if (auxDigit == Constants.AUX_DIGIT_3) {
            DPUpdater.setNoticeNumber(debtPosition.getPaymentDetail(),
                    Constants.AUX_DIGIT_3 + debtPosition.getPaymentDetail().getIuv());
        }
    }

    /**
     * Updates <code>paymentStatus</code> in <code>debtPosition</code>
     * 
     * @param debtPosition
     * @param paymentStatusEnum
     * @see DebtPosition
     * @see PaymentStatusEnum
     */
    public static void changePaymentStatus(DebtPosition debtPosition, PaymentStatusEnum paymentStatusEnum) {
        DPUpdater.setPaymentStatus(debtPosition.getPaymentDetail(), paymentStatusEnum);
    }
}
