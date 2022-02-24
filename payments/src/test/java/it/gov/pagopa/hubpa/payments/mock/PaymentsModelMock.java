package it.gov.pagopa.hubpa.payments.mock;

import it.gov.pagopa.hubpa.payments.model.PaymentsModel;

public class PaymentsModelMock {
    public final static PaymentsModel getMock() {
	PaymentsModel mock = new PaymentsModel();

	mock.getDebitors().add(DebitorModelMock.createDebitor1());
	mock.getDebitors().add(DebitorModelMock.createDebitor2());

	return mock;
    }
}
