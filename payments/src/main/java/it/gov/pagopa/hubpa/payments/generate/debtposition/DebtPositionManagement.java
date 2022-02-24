package it.gov.pagopa.hubpa.payments.generate.debtposition;

import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.debtposition.business.DebtPositionBusiness;
import it.gov.pagopa.hubpa.payments.generate.debtposition.enumeration.PaymentStatusEnum;

/**
 * Contains the methods for managing the <code>DebtPosition</code>
 */
public class DebtPositionManagement {
    private DebtPositionManagement() {
	throw new IllegalStateException("DebtPositionManagement class");
    }

    /**
     * Validates a debt position
     * 
     * @param debtPosition
     * @throws Exception
     * @see DebtPosition
     */
    public static void validate(DebtPosition debtPosition) {
        DebtPositionBusiness.validate(debtPosition);
    }

    /**
     * Updates <code>paymentStatus</code> in <code>debtPosition</code> making it
     * payable
     * 
     * @param debtPosition
     * @return DebtPosition with the changed status
     * @throws Exception
     * @see DebtPosition
     */
    public static DebtPosition makePayable(DebtPosition debtPosition) {
        DebtPositionBusiness.changePaymentStatus(debtPosition, PaymentStatusEnum.PAYABLE);
        return debtPosition;
    }

    /**
     * Updates <code>paymentStatus</code> in <code>debtPosition</code> making it
     * not payable
     * 
     * @param debtPosition
     * @return DebtPosition with the changed status
     * @throws Exception
     * @see DebtPosition
     */
    public static DebtPosition makeNotPayable(DebtPosition debtPosition) {
        DebtPositionBusiness.changePaymentStatus(debtPosition, PaymentStatusEnum.NOT_PAYABLE);
        return debtPosition;
    }

    /**
     * Updates <code>paymentStatus</code> in <code>debtPosition</code> making it
     * canceled
     * 
     * @param debtPosition
     * @return DebtPosition with the changed status
     * @throws Exception
     * @see DebtPosition
     */
    public static DebtPosition makeCancel(DebtPosition debtPosition) {
        DebtPositionBusiness.changePaymentStatus(debtPosition, PaymentStatusEnum.CANCELED);
        return debtPosition;
    }

    /**
     * Updates <code>paymentStatus</code> in <code>debtPosition</code> making it
     * paid
     * 
     * @param debtPosition
     * @return DebtPosition with the changed status
     * @throws Exception
     * @see DebtPosition
     */
    public static DebtPosition makePaid(DebtPosition debtPosition) {
        DebtPositionBusiness.changePaymentStatus(debtPosition, PaymentStatusEnum.PAID);
        return debtPosition;
    }
}