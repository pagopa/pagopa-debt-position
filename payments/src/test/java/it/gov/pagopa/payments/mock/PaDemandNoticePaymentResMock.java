package it.gov.pagopa.payments.mock;

import it.gov.pagopa.payments.model.partner.CtQrCode;
import it.gov.pagopa.payments.model.partner.PaDemandPaymentNoticeRequest;
import it.gov.pagopa.payments.model.partner.PaDemandPaymentNoticeResponse;
import it.gov.pagopa.payments.model.partner.StOutcome;

public class PaDemandNoticePaymentResMock {

    public static PaDemandPaymentNoticeResponse getMock() {
        PaDemandPaymentNoticeResponse mock = new PaDemandPaymentNoticeResponse();

        CtQrCode qrCode = new CtQrCode();
        qrCode.setFiscalCode("77777777777");
        qrCode.setNoticeNumber("311111111112222222");

        mock.setFiscalCodePA("77777777777");
        mock.setCompanyName("77777777777");
        mock.setOfficeName("officeName");
        mock.setPaymentDescription("payment");
        mock.setQrCode(qrCode);
        mock.setOutcome(StOutcome.OK);

        return mock;
    }
}
