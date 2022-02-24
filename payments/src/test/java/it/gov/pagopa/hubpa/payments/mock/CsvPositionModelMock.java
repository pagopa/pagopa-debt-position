package it.gov.pagopa.hubpa.payments.mock;

import java.math.BigDecimal;

import it.gov.pagopa.hubpa.payments.model.CsvPositionModel;

public class CsvPositionModelMock {
    public final static CsvPositionModel getMock() {
	CsvPositionModel mock = new CsvPositionModel();

	mock.setFiscalCode("MRDPLL54H17D542L");
	mock.setType(1);
	mock.setName("Mario");
	mock.setSurname("Rossi");
	mock.setPhone("3330987654");
	mock.setAddress("Via di novoli");
	mock.setNumber("50/2");
	mock.setArea("Firenze");
	mock.setProvince("FI");
	mock.setCountry("IT");
	mock.setEmail("mario@firenze.it");
	mock.setIdTenant("abcd");
	mock.setAmount(BigDecimal.TEN);
	mock.setInformation("WARNING");
	mock.setCap("50127");
	
	return mock;
    }
}
