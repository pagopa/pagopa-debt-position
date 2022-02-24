package it.gov.pagopa.hubpa.payments.mock;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZoneId;

import it.gov.pagopa.hubpa.payments.model.DebitorModel;
import it.gov.pagopa.hubpa.payments.model.PaymentOptionsModel;
import it.gov.pagopa.hubpa.payments.model.PaymentPositionModel;
import it.gov.pagopa.hubpa.payments.model.TransfersModel;

public class DebitorModelMock {
    
    public final static DebitorModel createDebitor1() {
	DebitorModel mock = new DebitorModel();
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

	mock.getPaymentPosition().add(createPaymentPositionMock());

	return mock;
    }
    public final static DebitorModel createDebitor2() {
	DebitorModel mock = new DebitorModel();
	mock.setFiscalCode("CFLLDR54H17D542H");
	mock.setType(1);
	mock.setName("Luca");
	mock.setSurname("Bianchi");
	mock.setPhone("347623454");
	mock.setAddress("Via baracca");
	mock.setNumber("10");
	mock.setArea("Firenze");
	mock.setProvince("FI");
	mock.setCountry("IT");
	mock.setEmail("bianchi@firenze.it");
	mock.setIdTenant("hjyu");

	mock.getPaymentPosition().add(createPaymentPositionMock());

	return mock;
    }
    private static PaymentPositionModel createPaymentPositionMock() {

	PaymentPositionModel mock = new PaymentPositionModel();
	mock.setAmount(new BigDecimal(200));
	mock.setCompanyName(null);
	mock.setDescription(null);
	mock.setJobId(1l);
	mock.setOfficeName(null);
	mock.setOrganizationFiscalCode("12345678901");
	mock.setStatus(1);

	mock.getPaymentOptions().add(createPaymentOptionsMock1());
	mock.getPaymentOptions().add(createPaymentOptionsMock2());
	mock.getPaymentOptions().add(createPaymentOptionsMock3());

	return mock;
    }

    private static PaymentOptionsModel createPaymentOptionsMock1() {

	PaymentOptionsModel mock = new PaymentOptionsModel();
	mock.setAmount(new BigDecimal(200));
	mock.setDuoDate(LocalDate.now(ZoneId.of("Europe/Paris")));
	mock.setFiscalCode("12345678901");
	mock.setIsConclusive(Boolean.TRUE);
	mock.setMetadata(null);
	mock.setRetentionDate(null);
	mock.setStatus(1);

	mock.getTransfers().add(createTransfersMock1a());
	mock.getTransfers().add(createTransfersMock1b());

	return mock;
    }

    private static PaymentOptionsModel createPaymentOptionsMock2() {

	PaymentOptionsModel mock = new PaymentOptionsModel();
	mock.setAmount(new BigDecimal(120));
	mock.setDuoDate(LocalDate.now(ZoneId.of("Europe/Paris")));
	mock.setFiscalCode("12345678901");
	mock.setIsConclusive(Boolean.FALSE);
	mock.setMetadata(null);
	mock.setRetentionDate(null);
	mock.setStatus(1);

	mock.getTransfers().add(createTransfersMock2a());

	return mock;
    }

    private static PaymentOptionsModel createPaymentOptionsMock3() {

	PaymentOptionsModel mock = new PaymentOptionsModel();
	mock.setAmount(new BigDecimal(80));
	mock.setDuoDate(LocalDate.now(ZoneId.of("Europe/Paris")));
	mock.setFiscalCode("12345678901");
	mock.setIsConclusive(Boolean.FALSE);
	mock.setMetadata(null);
	mock.setRetentionDate(null);
	mock.setStatus(1);

	mock.getTransfers().add(createTransfersMock3a());
	mock.getTransfers().add(createTransfersMock3b());

	return mock;
    }

    private static TransfersModel createTransfersMock1a() {
	TransfersModel mock = new TransfersModel();
	mock.setIban("IT12345677889");
	mock.setOrganizationFiscalCode("12345678901");
	mock.setPartialAmount(new BigDecimal(150));
	mock.setReason("causale tari tefa");
	mock.setTaxonomy("10/22252/20");
	mock.setPostalIban("000050570131");

	return mock;
    }

    private static TransfersModel createTransfersMock2a() {
	TransfersModel mock = new TransfersModel();
	mock.setIban("IT12345677889");
	mock.setOrganizationFiscalCode("12345678901");
	mock.setPartialAmount(new BigDecimal(120));
	mock.setReason("causale tari tefa");
	mock.setTaxonomy("10/22252/20");

	return mock;
    }

    private static TransfersModel createTransfersMock3a() {
	TransfersModel mock = new TransfersModel();
	mock.setIban("IT12345677889");
	mock.setOrganizationFiscalCode("12345678901");
	mock.setPartialAmount(new BigDecimal(75));
	mock.setReason("causale tari tefa");
	mock.setTaxonomy("10/22252/20");

	return mock;
    }

    private static TransfersModel createTransfersMock1b() {
	TransfersModel mock = new TransfersModel();
	mock.setIban("IT989999999");
	mock.setOrganizationFiscalCode("99999999999");
	mock.setPartialAmount(new BigDecimal(50));
	mock.setReason("causale tari tefa");
	mock.setTaxonomy("20/22252/20");

	return mock;
    }

    private static TransfersModel createTransfersMock3b() {
	TransfersModel mock = new TransfersModel();
	mock.setIban("IT989999999");
	mock.setOrganizationFiscalCode("99999999999");
	mock.setPartialAmount(new BigDecimal(50));
	mock.setReason("causale tari tefa");
	mock.setTaxonomy("20/22252/20");

	return mock;
    }

}
