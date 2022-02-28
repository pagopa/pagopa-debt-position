package it.gov.pagopa.payments.mock;

import it.gov.pagopa.payments.model.partner.CtQrCode;
import it.gov.pagopa.payments.model.partner.PaGetPaymentReq;

public class PaGetPaymentReqMock {

    public static PaGetPaymentReq getMock() {

        CtQrCode qrCode = new CtQrCode();
        qrCode.setFiscalCode("77777777777");
        qrCode.setNoticeNumber("311111111112222222");

        PaGetPaymentReq mock = new PaGetPaymentReq();
        mock.setIdBrokerPA("77777777777");
        mock.setIdPA("77777777777");
        mock.setIdStation("77777777777_01");
        mock.setQrCode(qrCode);

        return mock;
    }
}
