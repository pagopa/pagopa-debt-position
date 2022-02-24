package it.gov.pagopa.hubpa.payments.mock;

import it.gov.pagopa.hubpa.payments.model.partner.CtQrCode;
import it.gov.pagopa.hubpa.payments.model.partner.PaVerifyPaymentNoticeReq;

public class PaVerifyPaymentNoticeReqMock {

    public final static PaVerifyPaymentNoticeReq getMock() {

        CtQrCode qrCode = new CtQrCode();
        qrCode.setFiscalCode("77777777777");
        qrCode.setNoticeNumber("311111111112222222");

        PaVerifyPaymentNoticeReq mock = new PaVerifyPaymentNoticeReq();
        mock.setIdBrokerPA("77777777777");
        mock.setIdPA("77777777777");
        mock.setIdStation("77777777777_01");
        mock.setQrCode(qrCode);

        return mock;
    }
}
