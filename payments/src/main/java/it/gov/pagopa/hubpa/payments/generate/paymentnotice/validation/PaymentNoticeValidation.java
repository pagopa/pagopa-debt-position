package it.gov.pagopa.hubpa.payments.generate.paymentnotice.validation;

import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PaymentNotice;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.exception.ValidationException;

/**
 * Validation of PaymentNotice interface
 */
public interface PaymentNoticeValidation {

    /**
     * Validate the payment notice
     * 
     * @param paymentNotice
     *            the Payment Notice Bean
     * @throws ValidationException
     */
    void validate(PaymentNotice paymentNotice) throws ValidationException;

    /**
     * Constraints validation by annotation of the payment notice or one of its
     * components
     * 
     * @param objectToValidate
     * @throws ValidationException
     */
    <T> void checkConstraints(T objectToValidate) throws ValidationException;
}
