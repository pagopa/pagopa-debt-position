package it.gov.pagopa.hubpa.payments.mock;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.hubpa.payments.model.UploadCsvPartialModel;
import it.gov.pagopa.hubpa.payments.model.csv.CsvModel;
import it.gov.pagopa.hubpa.payments.model.csv.CsvRowModel;

public class UploadCsvPartialModelMock {
    public final static UploadCsvPartialModel getMock() {
	UploadCsvPartialModel mock = new UploadCsvPartialModel();
	mock.setFiscalCodeCreditor("12345678901");
	mock.setJobId(1l);
	mock.setCsv(createCsvModelMock());

	return mock;
    }

    private static CsvModel createCsvModelMock() {
	CsvModel mock = new CsvModel();
	mock.setFileName("fileProva.csv");

	mock.setRows(createCsvRowListModelMock());
	return mock;
    }

    private static List<CsvRowModel> createCsvRowListModelMock() {
	List<CsvRowModel> mockList = new ArrayList<>();

	CsvRowModel mock = new CsvRowModel();
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
	mock.setAmount(new BigDecimal(265));
	mock.setReason("Pagamento tari/tefa");
	mockList.add(mock);

	mock = new CsvRowModel();
	mock.setFiscalCode("MRDPLL54H17D542L");
	mock.setType(1);
	mock.setName("Fabio");
	mock.setSurname("Bianchi");
	mock.setPhone("586522621");
	mock.setAddress("Via di Canova");
	mock.setNumber("2");
	mock.setArea("Milano");
	mock.setProvince("MI");
	mock.setCountry("IT");
	mock.setEmail("fabio@milano.it");
	mock.setIdTenant("trty");
	mock.setAmount(new BigDecimal(489));
	mock.setReason("Pagamento tari/tefa milano");
	mockList.add(mock);

	return mockList;
    }

}
