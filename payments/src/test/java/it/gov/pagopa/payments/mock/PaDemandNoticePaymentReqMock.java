package it.gov.pagopa.payments.mock;

import it.gov.pagopa.payments.model.partner.CtQrCode;
import it.gov.pagopa.payments.model.partner.PaDemandPaymentNoticeRequest;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeReq;

public class PaDemandNoticePaymentReqMock {

    public static PaDemandPaymentNoticeRequest getMock() {
        PaDemandPaymentNoticeRequest mock = new PaDemandPaymentNoticeRequest();
        mock.setIdBrokerPA("77777777777");
        mock.setIdPA("77777777777");
        mock.setIdStation("77777777777_01");
        mock.setIdServizio("1");
        String xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
                "<!-- Created with Liquid Technologies Online Tools 1.0 (https://www.liquid-technologies.com) -->\n" +
                "<service xmlns=\"http://PuntoAccessoPSP.spcoop.gov.it/GeneralService\" xsi:schemaLocation=\"http://PuntoAccessoPSP.spcoop.gov.it/GeneralService schema.xsd\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n" +
                "  <AnyElementYouLike>Some Data Or Others Elements</AnyElementYouLike>\n" +
                "  <AnyElementYouLike1>Some Datsdfsda Or Other Elements</AnyElementYouLike1>\n" +
                "  <AnyElementYouLike2>sadfasd</AnyElementYouLike2>\n" +
                "  <ciao>Some Data Or Other Elements</ciao>\n" +
                "</service>";
        mock.setDatiSpecificiServizioRequest(xml.getBytes());

        return mock;
    }
}
