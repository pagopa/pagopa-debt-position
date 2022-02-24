package it.gov.pagopa.hubpa.payments.mock;

import it.gov.pagopa.hubpa.payments.model.PaymentJobMinimalModel;

public class PaymentJobMinimalModelMock {
    public final static PaymentJobMinimalModel getMock() {
	PaymentJobMinimalModel mock = new PaymentJobMinimalModel();

	mock.setJobId(1l);
	mock.setNRecordAdded(1);
	mock.setNRecordFound(1);
	mock.setNRecordWarning(1);
	mock.setStatus(4);

	return mock;
    }
}
