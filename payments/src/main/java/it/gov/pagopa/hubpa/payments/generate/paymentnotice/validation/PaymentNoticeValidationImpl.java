package it.gov.pagopa.hubpa.payments.generate.paymentnotice.validation;

import java.util.HashMap;
import java.util.Set;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;

import it.gov.pagopa.hubpa.payments.iuvgenerator.common.ErrorMessages;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PaymentNotice;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.PaymentNoticeBusiness;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.exception.ValidationException;

/**
 * Implementation of PaymentNoticeValidation interface
 */
public class PaymentNoticeValidationImpl implements PaymentNoticeValidation {

    /**
     * Validate the paymentNotice<br/>
     * The validation includes:
     * <ul>
     * <li>checkConstraints - validation by annotation
     * <li>checkInstallmentsDebtPosition - if size of debtPosition list = 1 then
     * <code>documentNumber</code> and <code>installmentNumber</code> must not
     * be valued; else all the expected <code>installmentNumber</code> and
     * <code>documentNumber</code> must be present
     * </ul>
     * 
     * @param paymentNotice
     *            the bean to validate
     * @throws ValidationException
     * @see PaymentNotice
     * @see ValidationException
     */
    @Override
    public void validate(PaymentNotice paymentNotice) throws ValidationException {
        checkConstraints(paymentNotice);

        checkInstallmentsDebtPosition(paymentNotice);
    }

    /**
     * @param objectToValidate
     *            the bean to validate. It can be the payment notice or one of
     *            its components.
     * @throws ValidationException
     * @see ValidationException
     */
    public <T> void checkConstraints(T objectToValidate) throws ValidationException {
        Validator validator = Validation.buildDefaultValidatorFactory().getValidator();
        Set<ConstraintViolation<T>> validationInputResults = validator.validate(objectToValidate);
        if (!validationInputResults.isEmpty()) {
            throw new ValidationException(validationInputResults);
        }
    }

    /**
     * @param paymentNotice
     * @see PaymentNotice
     */
    private void checkInstallmentsDebtPosition(PaymentNotice paymentNotice) {
        if (paymentNotice.getDebtPositionList().size() == 1) {
            if (paymentNotice.getDebtPositionList().get(0).getPaymentDetail().getDocumentNumber() != null) {
                throw new ValidationException(ErrorMessages.VALIDATION_INVALID_DOCUMENT_NUMBER);
            }
            if (paymentNotice.getDebtPositionList().get(0).getPaymentDetail().getInstallmentNumber() != null) {
                throw new ValidationException(ErrorMessages.VALIDATION_INVALID_INSTALLMENT_NUMBER);
            }
        } else {
            HashMap<Integer, DebtPosition> sortedDebtPositionHashMap = PaymentNoticeBusiness
                    .sortDebtPositionListByInstallmentNumberExcludingSingleInstallment(
                            paymentNotice.getDebtPositionList());
            for (int i = 1; i <= sortedDebtPositionHashMap.size(); i++) {
                if (sortedDebtPositionHashMap.get(i) == null) {
                    throw new ValidationException(ErrorMessages.VALIDATION_INSTALLMENTS_MISSING_INSTALLMENT_NUMBER + i);
                }
                if (sortedDebtPositionHashMap.get(i).getPaymentDetail().getDocumentNumber() == null) {
                    throw new ValidationException(ErrorMessages.VALIDATION_INSTALLMENTS_MISSING_DOCUMENT_NUMBER);
                }
            }
        }
    }
}
