package it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer;

import java.math.BigDecimal;

import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;

/**
 * Business logic for generating QR code
 */
public class QrCodeBusiness {
    private QrCodeBusiness() {
	throw new IllegalStateException("QrCodeBusiness class");
    }

    private static final String PAGOPA_QR_CODE_STRING = "PAGOPA|002|%1$s|%2$s|%3$s";
    private static final String DIGIT_OF_2 = "%02d";

    /**
     * Generates the QR code
     * 
     * @param debtPosition
     * @param creditorInstitutionFiscalCode
     *            fiscal code of Creditor Institution
     * @return
     * @see DebtPosition
     */
    public static String createQrCode(DebtPosition debtPosition, String creditorInstitutionFiscalCode) {
        BigDecimal amount = debtPosition.getPaymentDetail().getTotalAmountPayment();
        String noticeNumber = debtPosition.getPaymentDetail().getNoticeNumber();
        return createQrCode(amount, noticeNumber, creditorInstitutionFiscalCode);
    }

    /**
     * @param amount
     * @param noticeNumber
     * @param creditorInstitutionFiscalCode
     * @return
     */
    private static String createQrCode(BigDecimal amount, String noticeNumber, String creditorInstitutionFiscalCode) {
        String centsAmount = String.valueOf((amount.multiply(new BigDecimal(100)).intValue()));
        String centsAmountPadQr = String.format(DIGIT_OF_2, Integer.parseInt(centsAmount));
       return String.format(PAGOPA_QR_CODE_STRING, noticeNumber, creditorInstitutionFiscalCode,
                centsAmountPadQr);
    }
}
