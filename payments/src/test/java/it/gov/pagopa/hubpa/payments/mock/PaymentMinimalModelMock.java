package it.gov.pagopa.hubpa.payments.mock;

import java.time.LocalDate;
import java.time.ZoneId;

import it.gov.pagopa.hubpa.payments.model.PaymentMinimalModel;

public class PaymentMinimalModelMock {
    public final static PaymentMinimalModel getMock() {
	PaymentMinimalModel mock = new PaymentMinimalModel();

	mock.setId(1l);
	mock.setFiscalCode("MCFKLO45454545");
	mock.setName("MARIO");
	mock.setSurname("ROSSI");
	mock.setDate(LocalDate.now(ZoneId.of("Europe/Paris")));
	mock.setStatus(2);
	mock.setIsDuplicated(Boolean.TRUE);
	
	return mock;
    }
}
