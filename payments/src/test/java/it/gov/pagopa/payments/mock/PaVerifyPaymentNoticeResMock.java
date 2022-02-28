package it.gov.pagopa.payments.mock;

import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;

public class PaVerifyPaymentNoticeResMock {

    public static PaVerifyPaymentNoticeRes getMock() {

        PaVerifyPaymentNoticeRes mock = new PaVerifyPaymentNoticeRes();
        mock.setCompanyName("77777777777");
        mock.setOfficeName("officeName");
        mock.setFiscalCodePA("77777777777");
        mock.setPaymentDescription("payment");
        mock.setOfficeName("officeName");

        return mock;
    }
}
