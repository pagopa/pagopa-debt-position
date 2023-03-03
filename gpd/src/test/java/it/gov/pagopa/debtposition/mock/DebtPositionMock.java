package it.gov.pagopa.debtposition.mock;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;

import it.gov.pagopa.debtposition.dto.PaymentOptionDTO;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.dto.TransferDTO;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;



public class DebtPositionMock {
	public final static PaymentPositionDTO getMock1() {
		return createPaymentPositionMock1();
	}

	public final static PaymentPositionDTO getMock2() {
		return createPaymentPositionMultipleMock1();
	}

	public final static PaymentPositionDTO getMock3() {
		return createPaymentPositionMultipleMock2();
	}

	public final static PaymentPositionDTO getMock4() {
		return createPaymentPositionUpdateMock1();
	}
	
	public final static PaymentPositionDTO getMock5() {
		return createPaymentPositionMock3();
	}
	
	public final static PaymentPositionDTO getMock6() {
		return createPaymentPositionMock5();
	}
	
	public final static PaymentPositionDTO getMock7() {
		return createPaymentPositionMock6();
	}
	
	public final static PaymentOptionDTO getPayPOMock1() {
		return createPayForPaymentOptionMock1();
	}


	public final static PaymentPositionDTO get400Mock1() {
		return createPaymentPosition400Mock1();
	}

	public final static PaymentPositionDTO get400Mock2() {
		return createPaymentPosition400Mock2();
	}

	public final static PaymentPositionDTO get400Mock3() {
		return createPaymentPosition400Mock3();
	}
	
	public final static PaymentPositionDTO get400Mock4() {
		return createPaymentPosition400Mock4();
	}
	
	public final static PaymentPositionDTO get400Mock5() {
		return createPaymentPosition400Mock5();
	}
	
	public final static PaymentPositionDTO get400Mock6() {
		return createPaymentPosition400Mock6();
	}
	
	public final static PaymentPositionDTO get400Mock7() {
		return createPaymentPosition400Mock7();
	}
	
	public final static PaymentPositionDTO get400Mock8() {
		return createPaymentPosition400Mock8();
	}
	
	public final static PaymentOptionDTO getPayPO400Mock1() {
		return createPayForPaymentOption400Mock1();
	}
	
	public final static PaymentPositionDTO get409_Min_Due_Date_Mock1() {
		return createPaymentPosition409_Min_Due_Date_Mock1();
	}

	public final static PaymentPositionDTO get409_Valid_Date_Mock1() {
		return createPaymentPosition409_Valid_Date_Mock1();
	}


	public static PaymentPositionDTO createPaymentPositionMock1() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("MRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK1");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMock1());

		return pPMock;
	}

	public static PaymentPositionDTO createPaymentPositionMock2() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("MRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK1");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMock1());

		return pPMock;
	}

	public static PaymentPositionDTO createPaymentPositionMock3() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("PIVA12345678");
		pPMock.setType(Type.G);
		pPMock.setFullName("Cipriani Srl");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("RM");
		pPMock.setCountry("IT");
		pPMock.setEmail("cipriani@roma.it");
		pPMock.setPostalCode("00100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK3");
		pPMock.setCompanyName("Comune di Roma");
		pPMock.setOfficeName("Ufficio condono");
		pPMock.setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS));
		pPMock.addPaymentOptions(createPaymentOptionsMock1());

		return pPMock;
	}
	
	public static PaymentPositionDTO createPaymentPositionMock4() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("PIVA12345678");
		pPMock.setType(Type.G);
		pPMock.setFullName("Cipriani Srl");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("RM");
		pPMock.setCountry("IT");
		pPMock.setEmail("cipriani@roma.it");
		pPMock.setPostalCode("00100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK3");
		pPMock.setCompanyName("Comune di Roma");
		pPMock.setOfficeName("Ufficio condono");
		pPMock.setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS));
		pPMock.addPaymentOptions(createPaymentOptionsMock1());

		return pPMock;
	}
	
	public static PaymentPositionDTO createPaymentPositionMock5() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("PIVA12345678");
		pPMock.setType(Type.G);
		pPMock.setFullName("Cipriani Srl");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("RM");
		pPMock.setCountry("IT");
		pPMock.setEmail("cipriani@roma.it");
		pPMock.setPostalCode("00100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK5");
		pPMock.setCompanyName("Comune di Roma");
		pPMock.setOfficeName("Ufficio condono");
		pPMock.setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.DAYS));
		pPMock.addPaymentOptions(createPaymentOptionsMock6());

		return pPMock;
	}
	
	public static PaymentPositionDTO createPaymentPositionMock6() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("PIVA12345678");
		pPMock.setType(Type.G);
		pPMock.setFullName("Cipriani Srl");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("RM");
		pPMock.setCountry("IT");
		pPMock.setEmail("cipriani@roma.it");
		pPMock.setPostalCode("00100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK3");
		pPMock.setCompanyName("Comune di Roma");
		pPMock.setOfficeName("Ufficio condono");
		pPMock.setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS));
		pPMock.setSwitchToExpired(true);
		pPMock.addPaymentOptions(createPaymentOptionsMock1());

		return pPMock;
	}

	public static PaymentPositionDTO createPaymentPosition400Mock2() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		// manca il codice fiscale => deve dare errore 400
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK1");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMock1());

		return pPMock;
	}

	public static PaymentPositionDTO createPaymentPosition400Mock3() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("MRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK1");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMock2());

		return pPMock;
	}
	
	public static PaymentPositionDTO createPaymentPosition400Mock4() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("MRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK4");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMock3());

		return pPMock;
	}
	
	public static PaymentPositionDTO createPaymentPosition400Mock5() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("MRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK4");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMock4());

		return pPMock;
	}
	
	public static PaymentPositionDTO createPaymentPosition400Mock6() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("MRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK4");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMock4());
		// La validity date è minore della current date
		pPMock.setValidityDate(LocalDateTime.now(ZoneOffset.UTC).minus(1, ChronoUnit.DAYS));

		return pPMock;
	}
	
	public static PaymentPositionDTO createPaymentPosition400Mock7() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("MRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK4");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMock7());
		// La validity date è minore della current date
		pPMock.setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plus(1, ChronoUnit.DAYS));

		return pPMock;
	}
	
	public static PaymentPositionDTO createPaymentPosition400Mock8() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("MRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK4");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMock8());
		// La validity date è minore della current date
		pPMock.setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plus(1, ChronoUnit.DAYS));

		return pPMock;
	}
	
	public static PaymentPositionDTO createPaymentPosition409_Min_Due_Date_Mock1() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("MRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Mario Rossi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("FI");
		pPMock.setCountry("IT");
		pPMock.setEmail("mario@firenze.it");
		pPMock.setPostalCode("50100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK1");
		pPMock.setCompanyName("Comune di Firenze");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptions_Min_Due_Date_Mock1());
		pPMock.addPaymentOptions(createPaymentOptions_Min_Due_Date_Mock2());

		return pPMock;
	}

	public static PaymentPositionDTO createPaymentPosition409_Valid_Date_Mock1() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("PIVA12345678");
		pPMock.setType(Type.G);
		pPMock.setFullName("Cipriani Srl");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("RM");
		pPMock.setCountry("IT");
		pPMock.setEmail("cipriani@roma.it");
		pPMock.setPostalCode("00100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK4");
		pPMock.setCompanyName("Comune di Roma");
		pPMock.setOfficeName("Ufficio condono");
		pPMock.setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS));
		pPMock.addPaymentOptions(createPaymentOptionsMock1());

		return pPMock;
	}

	public static PaymentPositionDTO createPaymentPositionMultipleMock1() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("CPRPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Marco Bianchi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("RM");
		pPMock.setCountry("IT");
		pPMock.setEmail("marco@roma.it");
		pPMock.setPostalCode("00100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMULTIPLEMOCK1");
		pPMock.setCompanyName("Comune di Roma");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMultipleMock1());
		pPMock.addPaymentOptions(createPaymentOptionsMultipleMock2());

		return pPMock;
	}

	public static PaymentPositionDTO createPaymentPositionMultipleMock2() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("PIVA12345678");
		pPMock.setType(Type.G);
		pPMock.setFullName("Cipriani Srl");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("RM");
		pPMock.setCountry("IT");
		pPMock.setEmail("cipriani@roma.it");
		pPMock.setPostalCode("00100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMULTIPLEMOCK2");
		pPMock.setCompanyName("Comune di Roma");
		pPMock.setOfficeName("Ufficio condono");
		pPMock.addPaymentOptions(createPaymentOptionsMultipleMock3());
		pPMock.addPaymentOptions(createPaymentOptionsMultipleMock4());
		pPMock.addPaymentOptions(createPaymentOptionsMultipleMock5());

		return pPMock;
	}
	

	public static PaymentPositionDTO createPaymentPosition400Mock1() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("VRDPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Antonio Verdi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("RM");
		pPMock.setCountry("IT");
		pPMock.setEmail("antonio@roma.it");
		pPMock.setPostalCode("00100");
		// payment position properties
		pPMock.setIupd("12345678901IUPD400MOCK1");
		pPMock.setCompanyName("Comune di Roma");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.setStatus(DebtPositionStatus.DRAFT);
		pPMock.addPaymentOptions(createPaymentOptions400Mock1());

		return pPMock;
	}

	public static PaymentPositionDTO createPaymentPositionUpdateMock1() {

		PaymentPositionDTO pPMock = new PaymentPositionDTO();
		// debtor properties
		pPMock.setFiscalCode("CPRPLL54H17D542L");
		pPMock.setType(Type.F);
		pPMock.setFullName("Marco Bianchi");
		pPMock.setPhone("3330987654");
		pPMock.setStreetName("Via di novoli");
		pPMock.setCivicNumber("50/2");
		pPMock.setProvince("RM");
		pPMock.setCountry("IT");
		pPMock.setEmail("marco@roma.it");
		pPMock.setPostalCode("00100");
		// payment position properties
		pPMock.setIupd("12345678901IUPDMOCK1");
		pPMock.setCompanyName("Comune di Roma");
		pPMock.setOfficeName("Ufficio tributario");
		pPMock.addPaymentOptions(createPaymentOptionsMultipleMock1());
		pPMock.addPaymentOptions(createPaymentOptionsMultipleMock2());

		return pPMock;
	}



	public static PaymentOptionDTO createPaymentOptionsMock1() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMOCK1");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.SECONDS));
		pOMock.setRetentionDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMock1()); 

		return pOMock;
	}

	public static PaymentOptionDTO createPaymentOptionsMock2() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMOCK2");
		// due_date < current date => deve dare errore 400
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).minus(1, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMock1()); 

		return pOMock;
	}
	
	public static PaymentOptionDTO createPaymentOptionsMock3() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		// la PO ha un amount diverso dal trasfer associato
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMOCK3");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMock2()); 

		return pOMock;
	}
	
	public static PaymentOptionDTO createPaymentOptionsMock4() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMOCK4");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
		pOMock.setRetentionDate(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMock1()); 

		return pOMock;
	}
	
	public static PaymentOptionDTO createPaymentOptionsMock5() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMOCK1");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.SECONDS));
		pOMock.setRetentionDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMock1()); 

		return pOMock;
	}
	
	public static PaymentOptionDTO createPaymentOptionsMock6() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMOCK6");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
		pOMock.setRetentionDate(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMock1()); 

		return pOMock;
	}
	
	public static PaymentOptionDTO createPaymentOptionsMock7() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMOCK7");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
		pOMock.setRetentionDate(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMultipleMock1()); 
		pOMock.addTransfers(createTransfersMultipleMock2());
		pOMock.addTransfers(createTransfersMultipleMock3());
		pOMock.addTransfers(createTransfersMultipleMock4());
		pOMock.addTransfers(createTransfersMultipleMock5());
		pOMock.addTransfers(createTransfersMultipleMock6());

		return pOMock;
	}
	
	public static PaymentOptionDTO createPaymentOptionsMock8() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMOCK8");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
		pOMock.setRetentionDate(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMultipleMock1()); 
		pOMock.addTransfers(createTransfersMultipleMock2());
		pOMock.addTransfers(createTransfersMultipleMock4());
		pOMock.addTransfers(createTransfersMultipleMock5());
		pOMock.addTransfers(createTransfersMultipleMock6());

		return pOMock;
	}
	
	public static PaymentOptionDTO createPaymentOptions_Min_Due_Date_Mock1() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMOCK1");
		// due_date impostata di pochissimo maggiore della current date per passare il controllo
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(3, ChronoUnit.SECONDS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMultipleMock1()); 

		return pOMock;
	}
	
	public static PaymentOptionDTO createPaymentOptions_Min_Due_Date_Mock2() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(500);
		pOMock.setIuv("123456IUVMOCK2");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(1, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMultipleMock2()); 

		return pOMock;
	}

	public static PaymentOptionDTO createPaymentOptionsMultipleMock1() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUVMULTIPLEMOCK1");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMultipleMock1()); 

		return pOMock;
	}

	public static PaymentOptionDTO createPaymentOptionsMultipleMock2() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(500);
		pOMock.setIuv("123456IUVMULTIPLEMOCK2");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMultipleMock2()); 

		return pOMock;
	}

	public static PaymentOptionDTO createPaymentOptionsMultipleMock3() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(10000);
		pOMock.setIuv("123456IUVMULTIPLEMOCK3");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMultipleMock3()); 

		return pOMock;
	}

	public static PaymentOptionDTO createPaymentOptionsMultipleMock4() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(5000);
		pOMock.setIuv("123456IUVMULTIPLEMOCK4");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS));
		pOMock.setIsPartialPayment(Boolean.TRUE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMultipleMock4()); 
		pOMock.addTransfers(createTransfersMultipleMock5()); 

		return pOMock;
	}
	
	public static PaymentOptionDTO createPaymentOptionsMultipleMock5() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(5000);
		pOMock.setIuv("123456IUVMULTIPLEMOCK5");
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(4, ChronoUnit.HOURS));
		pOMock.setIsPartialPayment(Boolean.TRUE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMultipleMock4()); 
		pOMock.addTransfers(createTransfersMultipleMock5()); 

		return pOMock;
	}
	
	public static PaymentOptionDTO createPayForPaymentOptionMock1() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setPaymentDate(LocalDateTime.now(ZoneOffset.UTC));
		pOMock.setPaymentMethod("Bonifico");
		pOMock.setPspCompany("Intesa San Paolo");
		pOMock.setIdReceipt("TRN987654321");
		pOMock.setFee(0);

		return pOMock;
	}

	public static PaymentOptionDTO createPaymentOptions400Mock1() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setAmount(1000);
		pOMock.setIuv("123456IUV400MOCK1");
		// due_date < current date => deve dare errore 400
		pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).minus(1, ChronoUnit.DAYS));
		pOMock.setIsPartialPayment(Boolean.FALSE);
		pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
		pOMock.addTransfers(createTransfersMock1()); 

		return pOMock;
	}
	
	public static PaymentOptionDTO createPayForPaymentOption400Mock1() {

		PaymentOptionDTO pOMock = new PaymentOptionDTO();
		pOMock.setPaymentDate(LocalDateTime.now(ZoneOffset.UTC));
		pOMock.setPaymentMethod("Bonifico");
		pOMock.setPspCompany("Intesa San Paolo");
		// metto un campo obbligatorio a blank
		pOMock.setIdReceipt("");

		return pOMock;
	}
	
	


	public static TransferDTO createTransfersMock1() {
		TransferDTO tMock = new TransferDTO();
		tMock.setIdTransfer("1");
		tMock.setIban("IT75I0306902887100000300015");
		tMock.setAmount(1000);
		tMock.setRemittanceInformation("causale mock 1");
		tMock.setCategory("10/22252/20");
		tMock.setPostalIban("IT82E0760113600000000118547");

		return tMock;
	}
	
	public static TransferDTO createTransfersMock2() {
		TransferDTO tMock = new TransferDTO();
		tMock.setIdTransfer("1");
		tMock.setIban("IT75I0306902887100000300015");
		tMock.setAmount(10);
		tMock.setRemittanceInformation("causale mock 1");
		tMock.setCategory("10/22252/20");
		tMock.setPostalIban("IT82E0760113600000000118547");

		return tMock;
	}


	public static TransferDTO createTransfersMultipleMock1() {
		TransferDTO tMock = new TransferDTO();
		tMock.setIdTransfer("1");
		tMock.setIban("IT75I0306902887100000300005");
		tMock.setAmount(1000);
		tMock.setRemittanceInformation("causale mock multiple 1");
		tMock.setCategory("10/22252/20");
		tMock.setPostalIban("IT82E0760113600000000118547");

		return tMock;
	}

	public static TransferDTO createTransfersMultipleMock2() {
		TransferDTO tMock = new TransferDTO();
		tMock.setIdTransfer("2");
		tMock.setIban("IT75I0306902887100000300006");
		tMock.setAmount(500);
		tMock.setRemittanceInformation("causale mock multiple 2");
		tMock.setCategory("10/22252/20");
		tMock.setPostalIban("IT82E0760113600000000118547");

		return tMock;
	}

	public static TransferDTO createTransfersMultipleMock3() {
		TransferDTO tMock = new TransferDTO();
		tMock.setIdTransfer("3");
		tMock.setIban("IT75I0306902887100000300007");
		tMock.setAmount(10000);
		tMock.setRemittanceInformation("causale mock multiple 3");
		tMock.setCategory("10/22252/20");
		tMock.setPostalIban("IT82E0760113600000000118547");

		return tMock;
	}

	public static TransferDTO createTransfersMultipleMock4() {
		TransferDTO tMock = new TransferDTO();
		tMock.setIdTransfer("4");
		tMock.setIban("IT75I0306902887100000300007");
		tMock.setAmount(2500);
		tMock.setRemittanceInformation("causale mock multiple 4");
		tMock.setCategory("10/22252/20");
		tMock.setPostalIban("IT82E0760113600000000118547");

		return tMock;
	}

	public static TransferDTO createTransfersMultipleMock5() {
		TransferDTO tMock = new TransferDTO();
		tMock.setIdTransfer("5");
		tMock.setIban("IT75I0306902887100000300007");
		tMock.setAmount(2500);
		tMock.setRemittanceInformation("causale mock multiple 5");
		tMock.setCategory("10/22252/20");
		tMock.setPostalIban("IT82E0760113600000000118547");

		return tMock;
	}
	
	public static TransferDTO createTransfersMultipleMock6() {
		TransferDTO tMock = new TransferDTO();
		tMock.setIdTransfer("6");
		tMock.setIban("IT75I0306902887100000300007");
		tMock.setAmount(2500);
		tMock.setRemittanceInformation("causale mock multiple 5");
		tMock.setCategory("10/22252/20");
		tMock.setPostalIban("IT82E0760113600000000118547");

		return tMock;
	}

}