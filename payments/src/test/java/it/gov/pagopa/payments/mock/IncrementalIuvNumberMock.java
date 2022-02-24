package it.gov.pagopa.payments.mock;

import it.gov.pagopa.payments.entity.IncrementalIuvNumber;

public class IncrementalIuvNumberMock {
    public final static IncrementalIuvNumber getMock() {
	IncrementalIuvNumber mock = new IncrementalIuvNumber();
	mock.setAnno(2021);
	mock.setLastUsedNumber(3l);
	mock.setId(1l);
	mock.setIdDominioPa("12345678901");

	return mock;
    }
}
