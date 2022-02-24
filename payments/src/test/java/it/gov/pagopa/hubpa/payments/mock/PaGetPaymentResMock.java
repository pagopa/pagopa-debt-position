package it.gov.pagopa.hubpa.payments.mock;

import it.gov.pagopa.hubpa.payments.model.partner.CtPaymentPA;
import it.gov.pagopa.hubpa.payments.model.partner.PaGetPaymentRes;

public class PaGetPaymentResMock {

    public final static PaGetPaymentRes getMock() {

        PaGetPaymentRes mock = new PaGetPaymentRes();
        CtPaymentPA data = new CtPaymentPA();
        data.setCompanyName("company name");
        data.setCreditorReferenceId("id");
        mock.setData(data);

        return mock;
    }
}
