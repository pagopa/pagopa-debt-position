package it.gov.pagopa.hubpa.payments.mock;

import java.time.LocalDate;
import java.time.ZoneId;

import it.gov.pagopa.hubpa.payments.model.FilterModel;

public class FilterModelMock {
    public final static FilterModel getMock() {
	FilterModel mock = new FilterModel();

	mock.setTextSearch("123");
	mock.setStatus(1);
	mock.setDateTo(LocalDate.now(ZoneId.of("Europe/Paris")));
	mock.setDateFrom(LocalDate.now(ZoneId.of("Europe/Paris")));

	return mock;
    }
    public final static FilterModel getMock2() {
	return null;
    }
}
