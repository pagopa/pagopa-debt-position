package it.gov.pagopa.hubpa.payments.mock;

import it.gov.pagopa.hubpa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.hubpa.payments.model.partner.StOutcome;

public class PaSendRTResMock {

    public final static PaSendRTRes getMock() {

        PaSendRTRes mock = new PaSendRTRes();
        mock.setOutcome(StOutcome.OK);

        return mock;
    }
}
