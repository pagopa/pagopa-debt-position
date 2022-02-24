package it.gov.pagopa.payments.mock;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;

import it.gov.pagopa.payments.entity.Debitor;
import it.gov.pagopa.payments.entity.PaymentOptions;
import it.gov.pagopa.payments.entity.PaymentPosition;
import it.gov.pagopa.payments.entity.Transfers;
import it.gov.pagopa.payments.enumeration.PaymentOptionStatusEnum;
import it.gov.pagopa.payments.enumeration.PaymentStatusEnum;

public class DebitorMock {
  public final static Debitor getMock() {
    Debitor mock = new Debitor();
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

    mock.addPaymentPosition(createPaymentPositionMock());

    return mock;
  }
  

  public static PaymentPosition createPaymentPositionMock() {

    PaymentPosition mock = new PaymentPosition();
    mock.setAmount(new BigDecimal(200));
    mock.setCompanyName(null);
    mock.setDescription(null);
    mock.setInformation("Warning");
    mock.setInsertDate(LocalDateTime.now(ZoneId.of("Europe/Paris")));
    mock.setJobId(1l);
    mock.setOfficeName(null);
    mock.setOrganizationFiscalCode("12345678901");
    mock.setStatus(PaymentStatusEnum.BOZZA.getStatus());
    mock.setPublishDate(LocalDate.now());
    mock.addPaymentOptions(createPaymentOptionsMock1());
    mock.addPaymentOptions(createPaymentOptionsMock2());
    mock.addPaymentOptions(createPaymentOptionsMock3());

    mock.setTotalOptions(mock.getPaymentOptions().size());
    mock.setPaidOptions(0);
    mock.setReportedOptions(0);

    return mock;
  }

  public static PaymentPosition createPaymentPositionMock1() {

    PaymentPosition mock = new PaymentPosition();
    mock.setAmount(new BigDecimal(200));
    mock.setCompanyName("companyName");
    mock.setDescription("paymentDescription");
    mock.setInformation("Warning");
    mock.setInsertDate(LocalDateTime.now(ZoneId.of("Europe/Paris")));
    mock.setJobId(1l);
    mock.setOfficeName("officeName");
    mock.setOrganizationFiscalCode("77777777777");
    mock.setStatus(PaymentStatusEnum.PUBBLICATO.getStatus());
    mock.setPublishDate(LocalDate.now());

    // mock.addPaymentOptions(createPaymentOptionsMock4());

    mock.setTotalOptions(mock.getPaymentOptions().size());
    mock.setPaidOptions(0);
    mock.setReportedOptions(0);
    mock.setDebitor(DebitorMock.getMock());
    return mock;
  }

  public static PaymentOptions createPaymentOptionsMock4() {

    PaymentOptions mock = new PaymentOptions();
    mock.setAmount(new BigDecimal(200));
    mock.setDuoDate(LocalDate.now(ZoneId.of("Europe/Paris")));
    mock.setFiscalCode("77777777777");
    mock.setIsConclusive(Boolean.TRUE);
    mock.setMetadata(null);
    mock.setNotificationCode("311111111112222222");
    mock.setPaymentDate(null);
    mock.setRetentionDate(LocalDate.now(ZoneId.of("Europe/Paris")));
    mock.setStatus(PaymentOptionStatusEnum.NON_PAGATO.getStatus());
    mock.setAllCpp(Boolean.TRUE);

    mock.addTransfers(createTransfersMock1a());
    mock.addTransfers(createTransfersMock1b());
    mock.setPaymentPosition(createPaymentPositionMock1()); // PUBBLICATO

    return mock;
  }

  public static PaymentOptions createPaymentOptionsMock5() {

    PaymentOptions mock = new PaymentOptions();
    mock.setAmount(new BigDecimal(200));
    mock.setDuoDate(LocalDate.now(ZoneId.of("Europe/Paris")));
    mock.setFiscalCode("77777777777");
    mock.setIsConclusive(Boolean.TRUE);
    mock.setMetadata(null);
    mock.setNotificationCode("311111111112222222");
    mock.setPaymentDate(null);
    mock.setRetentionDate(null);
    mock.setStatus(PaymentOptionStatusEnum.PAGATO.getStatus());

    mock.addTransfers(createTransfersMock1a());
    mock.addTransfers(createTransfersMock1b());
    mock.setPaymentPosition(createPaymentPositionMock1()); // PUBBLICATO

    return mock;
  }

  public static Transfers createTransfersMock1a() {
    Transfers mock = new Transfers();
    mock.setIban("IT75I0306902887100000300015");
    mock.setOrganizationFiscalCode("12345678901");
    mock.setPartialAmount(new BigDecimal(150));
    mock.setReason("causale tari tefa");
    mock.setTaxonomy("10/22252/20");
    mock.setPostalIban("IT82E0760113600000000118547");

    return mock;
  }

  private static Transfers createTransfersMock2a() {
    Transfers mock = new Transfers();
    mock.setIban("IT75I0306902887100000300015");
    mock.setOrganizationFiscalCode("12345678901");
    mock.setPartialAmount(new BigDecimal(120));
    mock.setReason("causale tari tefa");
    mock.setTaxonomy("10/22252/20");

    return mock;
  }

  private static Transfers createTransfersMock3a() {
    Transfers mock = new Transfers();
    mock.setIban("IT75I0306902887100000300015");
    mock.setOrganizationFiscalCode("12345678901");
    mock.setPartialAmount(new BigDecimal(75));
    mock.setReason("causale tari tefa");
    mock.setTaxonomy("10/22252/20");

    return mock;
  }

  private static Transfers createTransfersMock1b() {
    Transfers mock = new Transfers();
    mock.setIban("IT75I0306902887100000300015");
    mock.setOrganizationFiscalCode("99999999999");
    mock.setPartialAmount(new BigDecimal(50));
    mock.setReason("causale tari tefa");
    mock.setTaxonomy("20/22252/20");

    return mock;
  }

  private static Transfers createTransfersMock3b() {
    Transfers mock = new Transfers();
    mock.setIban("IT75I0306902887100000300015");
    mock.setOrganizationFiscalCode("99999999999");
    mock.setPartialAmount(new BigDecimal(50));
    mock.setReason("causale tari tefa");
    mock.setTaxonomy("20/22252/20");

    return mock;
  }

  private static PaymentOptions createPaymentOptionsMock1() {

    PaymentOptions mock = new PaymentOptions();
    mock.setAmount(new BigDecimal(200));
    mock.setDuoDate(LocalDate.now(ZoneId.of("Europe/Paris")).plusDays(2));
    mock.setFiscalCode("12345678901");
    mock.setIsConclusive(Boolean.TRUE);
    mock.setMetadata(null);
    mock.setNotificationCode("022525561256120256");
    mock.setPaymentDate(null);
    mock.setRetentionDate(null);
    mock.setStatus(1);

    mock.setPaymentMethod("creditCard");
    mock.setPspCompanyName("Intesa San Paolo");
    mock.setFee(BigDecimal.valueOf(2));
    mock.setPaymentDate(LocalDateTime.of(2021, 06, 28, 0, 0));
    mock.setReceiptId("4444444");

    mock.addTransfers(createTransfersMock1a());
    mock.addTransfers(createTransfersMock1b());

    return mock;
  }

  private static PaymentOptions createPaymentOptionsMock2() {

    PaymentOptions mock = new PaymentOptions();
    mock.setAmount(new BigDecimal(120));
    mock.setDuoDate(LocalDate.now(ZoneId.of("Europe/Paris")).plusDays(32));
    mock.setFiscalCode("12345678901");
    mock.setIsConclusive(Boolean.FALSE);
    mock.setMetadata(null);
    mock.setNotificationCode("022525561256129875");
    mock.setPaymentDate(null);
    mock.setRetentionDate(null);
    mock.setStatus(1);

    mock.setPaymentMethod("creditCard");
    mock.setPspCompanyName("Intesa San Paolo");
    mock.setFee(BigDecimal.valueOf(2));
    mock.setPaymentDate(LocalDateTime.of(2021, 06, 28, 0, 0));
    mock.setReceiptId("123456");

    mock.addTransfers(createTransfersMock2a());

    return mock;
  }

  private static PaymentOptions createPaymentOptionsMock3() {

    PaymentOptions mock = new PaymentOptions();
    mock.setAmount(new BigDecimal(80));
    mock.setDuoDate(LocalDate.now(ZoneId.of("Europe/Paris")).plusDays(62));
    mock.setFiscalCode("12345678901");
    mock.setIsConclusive(Boolean.FALSE);
    mock.setMetadata(null);
    mock.setNotificationCode("022525561256123642");
    mock.setPaymentDate(null);
    mock.setRetentionDate(null);
    mock.setStatus(1);

    mock.setPaymentMethod("creditCard");
    mock.setPspCompanyName("Intesa San Paolo");
    mock.setFee(BigDecimal.valueOf(2));
    mock.setPaymentDate(LocalDateTime.of(2021, 06, 28, 0, 0));
    mock.setReceiptId("999");

    mock.addTransfers(createTransfersMock3a());
    mock.addTransfers(createTransfersMock3b());

    return mock;
  }

}
