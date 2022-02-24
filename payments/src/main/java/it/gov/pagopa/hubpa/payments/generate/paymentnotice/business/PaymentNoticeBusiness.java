package it.gov.pagopa.hubpa.payments.generate.paymentnotice.business;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PaymentNotice;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.validation.PaymentNoticeValidation;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.validation.PaymentNoticeValidationImpl;

/**
 * Business logic class
 */
public class PaymentNoticeBusiness {
    private PaymentNoticeBusiness() {
	throw new IllegalStateException("PaymentNoticeBusiness class");
    }

    /**
     * Validates a payment notice
     * 
     * @param paymentNotice
     * @see PaymentNotice
     */
    public static void validate(PaymentNotice paymentNotice) {
        PaymentNoticeValidation paymentNoticeValidation = new PaymentNoticeValidationImpl();
        paymentNoticeValidation.validate(paymentNotice);
    }

    /**
     * Constraints validation by annotation of the payment notice or one of its
     * components
     * 
     * @param objectToValidate
     *            the bean to validate. It can be the payment notice or one of
     *            its components.
     * @see PaymentNoticeValidationImpl
     * @see PaymentNotice
     */
    public static <T> void validateConstraints(T objectToValidate) {
        PaymentNoticeValidation paymentNoticeValidation = new PaymentNoticeValidationImpl();
        paymentNoticeValidation.checkConstraints(objectToValidate);
    }

    /**
     * Generates the pdf of <code>paymentNotice</code>
     * 
     * @param paymentNotice
     * @return the pdf in byte array
     * @throws IOException 
     * @see PaymentNotice
     */
    public static byte[] generatePaymentNotice(PaymentNotice paymentNotice) throws IOException {
        PdfPaymentNoticeCreator pdfPaymentNoticeCreator = new PdfPaymentNoticeCreator(paymentNotice);
        pdfPaymentNoticeCreator.createDocument();
        pdfPaymentNoticeCreator.closeStreams();
        return pdfPaymentNoticeCreator.getDocumentInBytes();
    }

    /**
     * Checks if <code>debtPosition</code> list contains only one installment
     * 
     * @param paymentNotice
     * @return true if <code>debtPosition</code> list has only one installment (
     *         "Rata Unica" ); false otherwise.
     * @see PaymentNotice
     */
    public static boolean hasSingleInstallment(PaymentNotice paymentNotice) {
        for (DebtPosition debtPosition : paymentNotice.getDebtPositionList()) {
            if (debtPosition.getPaymentDetail().getInstallmentNumber() == 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * Calculates the total amount of the payment even if the Single Installment
     * is not present
     * 
     * @param debtPositionList
     * @return <code>totalAmountPayment</code> of <code>DPPaymentDetail</code>
     *         if the size of <code>debtPosition</code> list is one or if the
     *         Single Installment is present, otherwise the sum of
     *         <code>totalAmountPayment</code> of <code>DPPaymentDetail</code>
     *         of all installments.
     * @see DebtPosition
     */
    public static BigDecimal getPaymentTotaleAmount(List<DebtPosition> debtPositionList) {
        BigDecimal totalAmountByInstallments = new BigDecimal(0);
        if (debtPositionList.size() == 1) {
            return debtPositionList.get(0).getPaymentDetail().getTotalAmountPayment();
        } else {
            for (DebtPosition paymentDataBean : debtPositionList) {
                if (paymentDataBean.getPaymentDetail().getInstallmentNumber() == 0) {
                    return paymentDataBean.getPaymentDetail().getTotalAmountPayment();
                }
                totalAmountByInstallments = totalAmountByInstallments
                        .add(paymentDataBean.getPaymentDetail().getTotalAmountPayment());
            }
        }
        return totalAmountByInstallments;
    }

    /**
     * Extracts <code>expirationDate</code> from <code>debtPosition</code> list
     * <br/>
     * 
     * @param debtPositionList
     * @return <code>expirationDate</code>
     * @see DebtPosition
     */
    public static Date getExpirationDate(List<DebtPosition> debtPositionList) {
        if (debtPositionList.size() == 1) {
            return debtPositionList.get(0).getPaymentDetail().getExpirationDate();
        } else {
            for (DebtPosition debtPosition : debtPositionList) {
                if (debtPosition.getPaymentDetail().getInstallmentNumber() == 0) {
                    return debtPosition.getPaymentDetail().getExpirationDate();
                }
            }
        }
        return null;
    }

    /**
     * Extracts the debt position for which the pdf section of the payment
     * notice must be compiled
     * 
     * @param debtPositionList
     * @return the <code>debtPosition</code> if the size of
     *         <code>debtPosition</code> list is one, otherwise the Single
     *         Installment ("Rata Unica")
     * @see DebtPosition
     */
    public static DebtPosition getReferenceDebtPosition(List<DebtPosition> debtPositionList) {
        if (debtPositionList.size() == 1) {
            return debtPositionList.get(0);
        } else {
            for (DebtPosition debtPosition : debtPositionList) {
                if (debtPosition.getPaymentDetail().getInstallmentNumber() == 0) {
                    return debtPosition;
                }
            }
        }
        return null;
    }

    /**
     * Computes the <code>noticeNumber</code> divided by 4 digits at a time
     * 
     * @param noticeNumber
     * @return the <code>noticeNumber</code> divided by 4 digits at a time
     */
    public static String getFormattedNoticeNumber(String noticeNumber) {
        return noticeNumber.substring(0, 4) + " " + noticeNumber.substring(4, 8) + " " + noticeNumber.substring(8, 12)
                + " " + noticeNumber.substring(12, 16) + " " + noticeNumber.substring(16, 18);
    }

    /**
     * Generates an ordered HashMap of <code>debtPosition</code> list excluding
     * Single Installment<br/>
     * <ul>
     * <li>HashMap key - <code>installmentNumber</code>
     * <li>HashMap value - <code>debtPosition</code>
     * </ul>
     * 
     * @param debtPositionList
     * @return ordered HashMap of <code>debtPosition</code>
     * @see DebtPosition
     */
    public static HashMap<Integer, DebtPosition> sortDebtPositionListByInstallmentNumberExcludingSingleInstallment(
            List<DebtPosition> debtPositionList) {
        HashMap<Integer, DebtPosition> map = new HashMap<>();
        for (DebtPosition debtPosition : debtPositionList) {
            if (debtPosition.getPaymentDetail().getInstallmentNumber() == 0) {
                continue;
            }
            map.put(debtPosition.getPaymentDetail().getInstallmentNumber(), debtPosition);
        }
        return map;
    }
}
