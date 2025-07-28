package it.gov.pagopa.debtposition.mock;

import it.gov.pagopa.debtposition.dto.MultiplePaymentPositionDTO;
import it.gov.pagopa.debtposition.dto.PaymentOptionDTO;
import it.gov.pagopa.debtposition.dto.PaymentOptionMetadataDTO;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.dto.TransferDTO;
import it.gov.pagopa.debtposition.dto.TransferMetadataDTO;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.NotificationFeeUpdateModel;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.RandomStringUtils;

public class DebtPositionMock {
  public static final PaymentPositionDTO getMock1() {
    return createPaymentPositionMock1();
  }

  public static final PaymentPositionDTO getMock2() {
    return createPaymentPositionMultipleMock1();
  }

  public static final PaymentPositionDTO getMock3() {
    return createPaymentPositionMultipleMock2();
  }

  public static final PaymentPositionDTO getMock4() {
    return createPaymentPositionUpdateMock1();
  }

  public static final PaymentPositionDTO getMock5() {
    return createPaymentPositionMock3();
  }

  public static final PaymentPositionDTO getMock6() {
    return createPaymentPositionMock5();
  }

  public static final PaymentPositionDTO getMock7() {
    return createPaymentPositionMock6();
  }

  public static final PaymentPositionDTO getMock8() {
    return createPaymentPositionMultipleMock8();
  }

  public static final PaymentPositionDTO getMock10() {
    return createPaymentPositionMock10();
  }

  public static final PaymentPositionDTO getMetadataMock8() {
    return createPaymentPositionMetadataMock7();
  }

  public static final PaymentOptionDTO getPayPOMock1() {
    return createPayForPaymentOptionMock1();
  }

  public static final PaymentPositionDTO get400Mock1() {
    return createPaymentPosition400Mock1();
  }

  public static final PaymentPositionDTO get400Mock2() {
    return createPaymentPosition400Mock2();
  }

  public static final PaymentPositionDTO get400Mock3() {
    return createPaymentPosition400Mock3();
  }

  public static final PaymentPositionDTO get400Mock4() {
    return createPaymentPosition400Mock4();
  }

  public static final PaymentPositionDTO get400Mock5() {
    return createPaymentPosition400Mock5();
  }

  public static final PaymentPositionDTO get400Mock6() {
    return createPaymentPosition400Mock6();
  }

  public static final PaymentPositionDTO get400Mock7() {
    return createPaymentPosition400Mock7();
  }

  public static final PaymentPositionDTO get400Mock8() {
    return createPaymentPosition400Mock8();
  }

  public static final PaymentPositionDTO get400Mock9() {
    return createPaymentPosition400Mock9();
  }

  public static final PaymentOptionDTO getPayPO400Mock1() {
    return createPayForPaymentOption400Mock1();
  }

  public static final PaymentPositionDTO get409_Min_Due_Date_Mock1() {
    return createPaymentPosition409_Min_Due_Date_Mock1();
  }

  public static final PaymentPositionDTO get409_Valid_Date_Mock1() {
    return createPaymentPosition409_Valid_Date_Mock1();
  }

  public static final MultiplePaymentPositionDTO getMultipleDebtPositions_Mock1() {
    return createMultipleDebtPositionsMock1();
  }

  public static final MultiplePaymentPositionDTO getMultipleDebtPositions_Mock2() {
    return createMultipleDebtPositionsMock2();
  }

  public static final MultiplePaymentPositionDTO getMultipleDebtPositions_400_Mock1() {
    return createMultipleDebtPositionsMock_400_1();
  }

  public static final MultiplePaymentPositionDTO getMultipleDebtPositions_400_Mock2() {
    return createMultipleDebtPositionsMock_400_2();
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

  public static PaymentPositionDTO createPaymentPositionMetadataMock7() {

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
    pPMock.setIupd("12345678901IUPDMETADATAMOCK7");
    pPMock.setCompanyName("Comune di Firenze");
    pPMock.setOfficeName("Ufficio tributario");
    pPMock.addPaymentOptions(createPaymentOptionsMetadataMock9());

    return pPMock;
  }

  public static PaymentPositionDTO createPaymentPositionMock10() {

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
    pPMock.setIupd("12345678901IUPDMOCK10_markd");
    pPMock.setCompanyName("Comune di Roma");
    pPMock.setOfficeName("Ufficio condono");
    pPMock.setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.DAYS));
    pPMock.addPaymentOptions(createPaymentOptionsMock10());

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

  public static PaymentPositionDTO createPaymentPosition400Mock9() {

    PaymentPositionDTO pPMock = new PaymentPositionDTO();
    // debtor properties
    pPMock.setFiscalCode("MRDPLL54H17D542L");
    pPMock.setType(Type.F);
    pPMock.setFullName("Mario Rossi");
    pPMock.setPhone("3330987654");
    pPMock.setStreetName("Via di novoli");
    pPMock.setCivicNumber("50/2");
    pPMock.setProvince("FI");
    pPMock.setCountry("Italia");
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

  public static PaymentPositionDTO createPaymentPositionMultipleMock8() {

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
    pPMock.setIupd("12345678901IUPDMULTIPLEMOCK8");
    pPMock.setCompanyName("Comune di Roma");
    pPMock.setOfficeName("Ufficio tributario");
    pPMock.addPaymentOptions(createPaymentOptionsMultipleMock8());

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

  public static PaymentOptionDTO createPaymentOption(
      int amount,
      String iuv,
      boolean isPartialPayment,
      TransferDTO transferDTO,
      LocalDateTime dueDate,
      LocalDateTime retentionDate) {
    PaymentOptionDTO poMock = new PaymentOptionDTO();
    poMock.setAmount(amount);
    poMock.setIuv(iuv);
    poMock.setDueDate(dueDate);
    poMock.setRetentionDate(retentionDate);
    poMock.setIsPartialPayment(isPartialPayment);
    poMock.setStatus(PaymentOptionStatus.PO_UNPAID);
    poMock.setDescription("payment option description");
    poMock.addTransfers(transferDTO);

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMock1() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMOCK1",
            false,
            createTransfersMock1(),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.SECONDS),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMock2() {
    // due_date < current date => trigger 400 BAD REQUEST
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMOCK2",
            false,
            createTransfersMock1(),
            LocalDateTime.now(ZoneOffset.UTC).minus(1, ChronoUnit.DAYS),
            null);

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMock3() {
    // Payment Option has a different amount from the associated transfer
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMOCK3",
            false,
            createTransfersMock2(),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS),
            null);

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMock4() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMOCK4",
            false,
            createTransfersMock1(),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS),
            LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.DAYS));

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMock6() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMOCK6",
            false,
            createTransfersMock1(),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS),
            LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMock7() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMOCK7",
            false,
            createTransfersMock1(),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS),
            LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));

    poMock.addTransfers(createTransfersMultipleMock2());
    poMock.addTransfers(createTransfersMultipleMock3());
    poMock.addTransfers(createTransfersMultipleMock4());
    poMock.addTransfers(createTransfersMultipleMock5());
    poMock.addTransfers(createTransfersMultipleMock6());

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMock8() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMOCK8",
            false,
            createTransfersMock1(),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS),
            LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
    poMock.addTransfers(createTransfersMultipleMock2());
    poMock.addTransfers(createTransfersMultipleMock4());
    poMock.addTransfers(createTransfersMultipleMock5());
    poMock.addTransfers(createTransfersMultipleMock6());

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMock10() {
    PaymentOptionDTO poMock =
            createPaymentOption(
                    1000,
                    "123456IUVMOCK10",
                    false,
                    createTransfersMock1(),
                    LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS),
                    LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionMock9(
      int amount,
      String iuv,
      boolean isPartialPayment,
      List<TransferDTO> transfersDTO,
      LocalDateTime dueDate,
      LocalDateTime retentionDate) {
    PaymentOptionDTO poMock = new PaymentOptionDTO();
    poMock.setAmount(amount);
    poMock.setIuv(iuv);
    poMock.setDueDate(dueDate);
    poMock.setRetentionDate(retentionDate);
    poMock.setIsPartialPayment(isPartialPayment);
    poMock.setStatus(PaymentOptionStatus.PO_UNPAID);
    poMock.setDescription("payment option description");
    poMock.setTransfer(transfersDTO);

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMetadataMock9() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMETADATAMOCK9",
            false,
            createTransfersMetadataMock3(),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
    poMock.addPaymentOptionMetadata(
        PaymentOptionMetadataDTO.builder()
            .key("keypometadatamock9")
            .value("valuepometadatamock9")
            .build());

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptions_Min_Due_Date_Mock1() {
    // due_date set very slightly greater than current date to pass control
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMOCK1",
            false,
            createTransfersMultipleMock1(),
            LocalDateTime.now(ZoneOffset.UTC).plus(3, ChronoUnit.SECONDS),
            null);

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptions_Min_Due_Date_Mock2() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            500,
            "123456IUVMOCK2",
            false,
            createTransfersMultipleMock2(),
            LocalDateTime.now(ZoneOffset.UTC).plus(1, ChronoUnit.DAYS),
            null);

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMultipleMock1() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            1000,
            "123456IUVMULTIPLEMOCK1",
            false,
            createTransfersMultipleMock1(),
            LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS),
            null);

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMultipleMock2() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            500,
            "123456IUVMULTIPLEMOCK2",
            false,
            createTransfersMultipleMock2(),
            LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS),
            null);

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMultipleMock3() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            10000,
            "123456IUVMULTIPLEMOCK3",
            false,
            createTransfersMultipleMock3(),
            LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS),
            null);

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMultipleMock4() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            5000,
            "123456IUVMULTIPLEMOCK4",
            true,
            createTransfersMultipleMock4(),
            LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS),
            null);
    poMock.addTransfers(createTransfersMultipleMock5());

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMultipleMock5() {
    PaymentOptionDTO poMock =
        createPaymentOption(
            5000,
            "123456IUVMULTIPLEMOCK5",
            true,
            createTransfersMultipleMock4(),
            LocalDateTime.now(ZoneOffset.UTC).plus(4, ChronoUnit.HOURS),
            null);
    poMock.addTransfers(createTransfersMultipleMock5());

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptionsMultipleMock8() {
    PaymentOptionDTO poMock =
        createPaymentOptionMock9(
            2000,
            "123456IUVMULTIPLEMOCK8",
            false,
            createTransfersMultipleMock8(),
            LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS),
            null);

    return poMock;
  }

  public static PaymentOptionDTO createPayForPaymentOptionMock1() {
    PaymentOptionDTO poMock = new PaymentOptionDTO();
    poMock.setPaymentDate(LocalDateTime.now(ZoneOffset.UTC));
    poMock.setPaymentMethod("Bonifico");
    poMock.setPspCompany("Intesa San Paolo");
    poMock.setIdReceipt("TRN987654321");
    poMock.setFee(0);
    poMock.setDescription("Description");

    return poMock;
  }

  public static PaymentOptionDTO createPaymentOptions400Mock1() {
    PaymentOptionDTO poMock = new PaymentOptionDTO();
    poMock.setAmount(1000);
    poMock.setIuv("123456IUV400MOCK1");
    // due_date < current date => must trigger 400 BAD_REQUEST
    poMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).minus(1, ChronoUnit.DAYS));
    poMock.setIsPartialPayment(Boolean.FALSE);
    poMock.setStatus(PaymentOptionStatus.PO_UNPAID);
    poMock.addTransfers(createTransfersMock1());
    poMock.setDescription("Description");

    return poMock;
  }

  public static PaymentOptionDTO createPayForPaymentOption400Mock1() {
    PaymentOptionDTO poMock = new PaymentOptionDTO();
    poMock.setPaymentDate(LocalDateTime.now(ZoneOffset.UTC));
    poMock.setPaymentMethod("Bonifico");
    poMock.setPspCompany("Intesa San Paolo");
    // metto un campo obbligatorio a blank
    poMock.setIdReceipt("");
    poMock.setDescription("Description");

    return poMock;
  }

  public static TransferDTO createTransfersMock1() {
    TransferDTO tMock = new TransferDTO();
    tMock.setIdTransfer("1");
    tMock.setCompanyName("mock company name");
    tMock.setIban("IT75I0306902887100000300015");
    tMock.setAmount(1000);
    tMock.setRemittanceInformation("causale mock 1");
    tMock.setCategory("10/22252/20");

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

  public static TransferDTO createTransfersMetadataMock3() {
    TransferDTO tMock = new TransferDTO();
    tMock.setIdTransfer("3");
    tMock.setIban("IT75I0306902887100000300015");
    tMock.setAmount(1000);
    tMock.setRemittanceInformation("causale metadata mock 3");
    tMock.setCategory("10/22252/20");
    tMock.addTransferMetadata(
        TransferMetadataDTO.builder()
            .key("keytransfermetadatamock3")
            .value("valuetransfermetadatamock3")
            .build());

    return tMock;
  }

  public static TransferDTO createTransfersMultipleMock1() {
    TransferDTO tMock = new TransferDTO();
    tMock.setIdTransfer("1");
    tMock.setIban("IT75I0306902887100000300005");
    tMock.setAmount(1000);
    tMock.setRemittanceInformation("causale mock multiple 1");
    tMock.setCategory("10/22252/20");

    return tMock;
  }

  public static TransferDTO createTransfersMultipleMock2() {
    TransferDTO tMock = new TransferDTO();
    tMock.setIdTransfer("2");
    tMock.setAmount(500);
    tMock.setRemittanceInformation("causale mock multiple 2");
    tMock.setCategory("10/22252/20");
    tMock.setIban("IT75I0306902887100000300005");
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

    return tMock;
  }

  public static TransferDTO createTransfersMultipleMock4() {
    TransferDTO tMock = new TransferDTO();
    tMock.setIdTransfer("4");
    tMock.setIban("IT75I0306902887100000300007");
    tMock.setAmount(2500);
    tMock.setRemittanceInformation("causale mock multiple 4");
    tMock.setCategory("10/22252/20");

    return tMock;
  }

  public static TransferDTO createTransfersMultipleMock5() {
    TransferDTO tMock = new TransferDTO();
    tMock.setIdTransfer("5");
    tMock.setIban("IT75I0306902887100000300007");
    tMock.setAmount(2500);
    tMock.setRemittanceInformation("causale mock multiple 5");
    tMock.setCategory("10/22252/20");

    return tMock;
  }

  public static TransferDTO createTransfersMultipleMock6() {
    TransferDTO tMock = new TransferDTO();
    tMock.setIdTransfer("6");
    tMock.setIban("IT75I0306902887100000300007");
    tMock.setAmount(2500);
    tMock.setRemittanceInformation("causale mock multiple 5");
    tMock.setCategory("10/22252/20");

    return tMock;
  }

  public static List<TransferDTO> createTransfersMultipleMock8() {
    List<TransferDTO> transfers = new ArrayList<>();
    TransferDTO tMock1 = new TransferDTO();
    tMock1.setIdTransfer("1");
    tMock1.setIban("IT75I0306902887100000300008");
    tMock1.setAmount(1000);
    tMock1.setRemittanceInformation("causale mock multiple 8a");
    tMock1.setCategory("10/22252/20");

    TransferDTO tMock2 = new TransferDTO();
    tMock2.setIdTransfer("2");
    tMock2.setIban("IT75I0306902887100000300088");
    tMock2.setAmount(1000);
    tMock2.setRemittanceInformation("causale mock multiple 8b");
    tMock2.setCategory("10/22252/21");

    transfers.add(tMock1);
    transfers.add(tMock2);

    return transfers;
  }

  public static PaymentPositionDTO paymentPositionForNotificationUpdateMock1() {
    PaymentPositionDTO mock = createPaymentPositionMock1();
    mock.getPaymentOption().get(0).setNotificationFee(300L);
    return mock;
  }

  public static NotificationFeeUpdateModel createNotificationFeeMock(long notificationFee) {
    NotificationFeeUpdateModel mock = new NotificationFeeUpdateModel();
    mock.setNotificationFee(notificationFee);
    return mock;
  }

  public static MultiplePaymentPositionDTO createMultipleDebtPositionsMock1() {
    List<PaymentPositionDTO> pPMockList = new ArrayList<>();
    PaymentPositionDTO pp = createPaymentPositionMock5();
    pp.setIupd(RandomStringUtils.randomNumeric(20));
    pp.getPaymentOption().get(0).setIuv(RandomStringUtils.randomNumeric(17));
    pPMockList.add(pp);
    return MultiplePaymentPositionDTO.builder().paymentPositions(pPMockList).build();
  }

  public static MultiplePaymentPositionDTO createMultipleDebtPositionsMock2() {
    List<PaymentPositionDTO> pPMockList = new ArrayList<>();
    PaymentPositionDTO pp = createPaymentPositionMock2();
    pp.setIupd(RandomStringUtils.randomNumeric(20));
    pp.getPaymentOption().get(0).setIuv(RandomStringUtils.randomNumeric(17));
    pPMockList.add(pp);
    return MultiplePaymentPositionDTO.builder().paymentPositions(pPMockList).build();
  }

  public static MultiplePaymentPositionDTO createMultipleDebtPositionsMock_400_1() {
    List<PaymentPositionDTO> pPMockList = new ArrayList<>();
    pPMockList.add(createPaymentPosition400Mock1());
    return MultiplePaymentPositionDTO.builder().paymentPositions(pPMockList).build();
  }

  public static MultiplePaymentPositionDTO createMultipleDebtPositionsMock_400_2() {
    List<PaymentPositionDTO> pPMockList = new ArrayList<>();
    pPMockList.add(createPaymentPosition400Mock2());
    return MultiplePaymentPositionDTO.builder().paymentPositions(pPMockList).build();
  }
}
