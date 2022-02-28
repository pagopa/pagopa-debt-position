package it.gov.pagopa.payments.mock;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import it.gov.pagopa.payments.model.partner.CtReceipt;
import it.gov.pagopa.payments.model.partner.PaSendRTReq;
import it.gov.pagopa.payments.model.partner.StOutcome;

public class PaSendRTReqMock {

    public static PaSendRTReq getMock() throws DatatypeConfigurationException {

        CtReceipt receipt = new CtReceipt();
        receipt.setReceiptId("c110729d258c4ab1b765fe902aae41d6");
        receipt.setNoticeNumber("311111111112222222");
        receipt.setFiscalCode("77777777777");
        receipt.setOutcome(StOutcome.OK);
        receipt.setCreditorReferenceId("11111111112222222");
        receipt.setPaymentMethod("creditCard");
        receipt.setPSPCompanyName("Intesa San Paolo");
        receipt.setFee(BigDecimal.valueOf(2));
        receipt.setPaymentDateTime(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(LocalDateTime.now().toString()));

        PaSendRTReq mock = new PaSendRTReq();
        mock.setIdBrokerPA("77777777777");
        mock.setIdPA("77777777777");
        mock.setIdStation("77777777777_01");
        mock.setReceipt(receipt);

        return mock;
    }
}
