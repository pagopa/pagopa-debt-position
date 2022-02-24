package it.gov.pagopa.hubpa.payments.mock;

import it.gov.pagopa.hubpa.payments.model.FindModel;

public class FindModelMock {
    public final static FindModel getMock() {
	FindModel mock = new FindModel();

	mock.setFiscalCode("12345678901");
	mock.setPage(0);
	mock.setSize(50);
	mock.setFilters(FilterModelMock.getMock());
	
	return mock;
    }
}
