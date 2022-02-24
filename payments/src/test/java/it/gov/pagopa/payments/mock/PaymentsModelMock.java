package it.gov.pagopa.payments.mock;

import it.gov.pagopa.payments.model.PaymentsModel;

public class PaymentsModelMock {
    public final static PaymentsModel getMock() {
	PaymentsModel mock = new PaymentsModel();

	mock.getDebitors().add(DebitorModelMock.createDebitor1());
	mock.getDebitors().add(DebitorModelMock.createDebitor2());

	return mock;
    }
}
