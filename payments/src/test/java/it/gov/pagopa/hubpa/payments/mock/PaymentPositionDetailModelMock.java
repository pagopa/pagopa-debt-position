package it.gov.pagopa.hubpa.payments.mock;

import java.time.LocalDate;

import it.gov.pagopa.hubpa.payments.model.PaymentPositionDetailModel;

public class PaymentPositionDetailModelMock {
    public final static PaymentPositionDetailModel getMock() {
	PaymentPositionDetailModel mock = new PaymentPositionDetailModel();

	mock.setAddressLine1("Via Mariti 22");
	mock.setAddressLine2("50127, Firenze");
	mock.setDescription("Taritefa2021");
	mock.setFiscalCode("SDE32323");
	mock.setNominative("Mario Rossi");
	mock.setStatus(1);
	mock.setPublishDate(LocalDate.now());
	return mock;
    }
}
