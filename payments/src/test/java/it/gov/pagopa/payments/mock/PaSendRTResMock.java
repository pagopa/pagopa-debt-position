package it.gov.pagopa.payments.mock;

import it.gov.pagopa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.payments.model.partner.StOutcome;

public class PaSendRTResMock {

    public static PaSendRTRes getMock() {

        PaSendRTRes mock = new PaSendRTRes();
        mock.setOutcome(StOutcome.OK);

        return mock;
    }
}
