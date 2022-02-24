package it.gov.pagopa.payments.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;

import java.util.Optional;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import it.gov.pagopa.payments.mock.PaGetPaymentReqMock;
import it.gov.pagopa.payments.mock.PaSendRTReqMock;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import it.gov.pagopa.payments.endpoints.validation.PaymentValidator;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.entity.PaymentOptions;
import it.gov.pagopa.payments.mock.DebitorMock;
import it.gov.pagopa.payments.mock.PaVerifyPaymentNoticeReqMock;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import it.gov.pagopa.payments.model.partner.ObjectFactory;
import it.gov.pagopa.payments.model.partner.PaGetPaymentReq;
import it.gov.pagopa.payments.model.partner.PaGetPaymentRes;
import it.gov.pagopa.payments.model.partner.PaSendRTReq;
import it.gov.pagopa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeReq;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;
import it.gov.pagopa.payments.model.partner.StOutcome;
import it.gov.pagopa.payments.model.partner.StTransferType;
import it.gov.pagopa.payments.model.partner.StAmountOption;
import it.gov.pagopa.payments.repository.DebitorRepository;
import it.gov.pagopa.payments.repository.IncrementalIuvNumberRepository;
import it.gov.pagopa.payments.repository.PaymentOptionsRepository;

@ExtendWith(MockitoExtension.class)
class PartnerServiceTest {

  @InjectMocks
  private PartnerService partnerService;

  @Mock
  DebitorRepository debitorRepository;

  @Mock
  PaymentOptionsRepository paymentOptionRepository;

  @Mock
  IncrementalIuvNumberRepository incrementalIuvNumberRepository;

  @Mock
  PaymentValidator paymentValidator;

  @Mock
  private ObjectFactory factory;

  private ObjectFactory factoryUtil = new ObjectFactory();

  @Test
  void paVerifyPaymentNoticeTest() throws DatatypeConfigurationException {

    // Test preconditions
    PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

    when(factory.createPaVerifyPaymentNoticeRes()).thenReturn(factoryUtil.createPaVerifyPaymentNoticeRes());
    when(factory.createCtPaymentOptionDescriptionPA()).thenReturn(factoryUtil.createCtPaymentOptionDescriptionPA());
    when(factory.createCtPaymentOptionsDescriptionListPA())
        .thenReturn(factoryUtil.createCtPaymentOptionsDescriptionListPA());

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(DebitorMock.createPaymentOptionsMock4()));

    // Test execution
    PaVerifyPaymentNoticeRes responseBody = partnerService.paVerifyPaymentNotice(requestBody);

    // Test postcondiction
    assertThat(responseBody.getOutcome()).isEqualTo(StOutcome.OK);
    assertThat(responseBody.getPaymentList().getPaymentOptionDescription().get(0).isAllCCP()).isTrue();
    assertThat(responseBody.getPaymentList().getPaymentOptionDescription().get(0).getAmount())
        .isEqualTo(DebitorMock.createPaymentOptionsMock4().getAmount());
    assertThat(responseBody.getPaymentList().getPaymentOptionDescription().get(0).getOptions())
        .isEqualTo(StAmountOption.EQ); // de-scoping
    assertThat(responseBody.getFiscalCodePA()).isEqualTo("77777777777");
    assertThat(responseBody.getOfficeName()).isEqualTo("officeName");
    assertThat(responseBody.getPaymentDescription()).isEqualTo("paymentDescription");
  }

  @Test
  void paVerifyPaymentNoticeTestKOsconosciuto() throws DatatypeConfigurationException {

    // Test preconditions
    PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();
    PaymentOptions option = DebitorMock.createPaymentOptionsMock5();

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));

    doThrow(new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO)).when(paymentValidator)
        .isPayable(option.getPaymentPosition(), option);

    // Test execution
    try {

      partnerService.paVerifyPaymentNotice(requestBody);

    } catch (PartnerValidationException e) {
      // Test postcondiction
      assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO.getFaultCode());
      assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO.getDescription());
      assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO.getFaultString());
    }
  }

  @Test
  void paVerifyPaymentNoticeTestKOpagato() throws DatatypeConfigurationException {

    // Test preconditions
    PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();
    PaymentOptions option = DebitorMock.createPaymentOptionsMock5();

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));

    doThrow(new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO)).when(paymentValidator)
        .isPayable(option.getPaymentPosition(), option);

    // Test execution
    try {

      partnerService.paVerifyPaymentNotice(requestBody);

    } catch (PartnerValidationException e) {
      // Test postcondiction
      assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO.getFaultCode());
      assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO.getDescription());
      assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO.getFaultString());
    }

  }

  @Test
  void paVerifyPaymentNoticeTestKOdominio() throws DatatypeConfigurationException {

    // Test preconditions
    PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();
    PaymentOptions option = DebitorMock.createPaymentOptionsMock5();

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));

    doThrow(new PartnerValidationException(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO)).when(paymentValidator).isAuthorize(
        requestBody.getIdPA(), requestBody.getIdBrokerPA(), requestBody.getIdStation(), option.getFiscalCode());

    // Test execution
    try {

      partnerService.paVerifyPaymentNotice(requestBody);

    } catch (PartnerValidationException e) {
      // Test postcondiction
      assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getFaultCode());
      assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getDescription());
      assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getFaultString());
    }

  }

  @Test
  void paVerifyPaymentNoticeTestKOintermediario() throws DatatypeConfigurationException {

    // Test preconditions
    PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

    PaymentOptions option = DebitorMock.createPaymentOptionsMock5();

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));

    doThrow(new PartnerValidationException(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO)).when(paymentValidator)
        .isAuthorize(requestBody.getIdPA(), requestBody.getIdBrokerPA(), requestBody.getIdStation(),
            option.getFiscalCode());

    // Test execution
    try {

      partnerService.paVerifyPaymentNotice(requestBody);

    } catch (PartnerValidationException e) {
      // Test postcondiction
      assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultCode());
      assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getDescription());
      assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultString());
    }

  }

  @Test
  void paVerifyPaymentNoticeTestKOstazione() throws DatatypeConfigurationException {

    // Test preconditions
    PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

    PaymentOptions option = DebitorMock.createPaymentOptionsMock5();

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));

    doThrow(new PartnerValidationException(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO)).when(paymentValidator).isAuthorize(
        requestBody.getIdPA(), requestBody.getIdBrokerPA(), requestBody.getIdStation(), option.getFiscalCode());

    // Test execution
    try {

      partnerService.paVerifyPaymentNotice(requestBody);

    } catch (PartnerValidationException e) {
      // Test postcondiction
      assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getFaultCode());
      assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getDescription());
      assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getFaultString());
    }
  }

  @Test
  void paGetPaymentTest() throws PartnerValidationException, DatatypeConfigurationException {

    // Test preconditions
    PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();
    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();

    when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
    when(factory.createCtPaymentPA()).thenReturn(factoryUtil.createCtPaymentPA());
    when(factory.createCtSubject()).thenReturn(factoryUtil.createCtSubject());
    when(factory.createCtEntityUniqueIdentifier()).thenReturn(factoryUtil.createCtEntityUniqueIdentifier());
    when(factory.createCtTransferListPA()).thenReturn(factoryUtil.createCtTransferListPA());
    when(factory.createCtTransferPA()).thenReturn(factoryUtil.createCtTransferPA());

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));

    // Test execution
    PaGetPaymentRes responseBody = partnerService.paGetPayment(requestBody);

    // Test postcondiction
    assertThat(responseBody.getData().getCompanyName()).isEqualTo(option.getPaymentPosition().getCompanyName());
    assertThat(responseBody.getData().getCreditorReferenceId()).isEqualTo(option.getNotificationCode().substring(1));
    assertThat(responseBody.getData().getDebtor().getFullName())
        .isEqualTo(option.getPaymentPosition().getDebitor().getName());
    assertThat(responseBody.getData().getDebtor().getCity())
        .isEqualTo(option.getPaymentPosition().getDebitor().getArea());
    assertThat(responseBody.getData().getDebtor().getCivicNumber())
        .isEqualTo(option.getPaymentPosition().getDebitor().getNumber());
    assertThat(responseBody.getData().getDebtor().getCountry())
        .isEqualTo(option.getPaymentPosition().getDebitor().getCountry());
    assertThat(responseBody.getData().getDebtor().getEMail())
        .isEqualTo(option.getPaymentPosition().getDebitor().getEmail());
    assertThat(responseBody.getData().getDebtor().getPostalCode())
        .isEqualTo(option.getPaymentPosition().getDebitor().getCap());
    assertThat(responseBody.getData().getDebtor().getStateProvinceRegion())
        .isEqualTo(option.getPaymentPosition().getDebitor().getProvince());
    assertThat(responseBody.getData().getDebtor().getStreetName())
        .isEqualTo(option.getPaymentPosition().getDebitor().getAddress());
    assertThat(responseBody.getData().getDebtor().getUniqueIdentifier().getEntityUniqueIdentifierValue())
        .isEqualTo(option.getPaymentPosition().getDebitor().getFiscalCode());
    assertThat(responseBody.getData().getDebtor().getUniqueIdentifier().getEntityUniqueIdentifierType().value())
        .isEqualTo(option.getPaymentPosition().getDebitor().getType().equals(1) ? "F" : "G");
    assertThat(responseBody.getData().getDescription()).isEqualTo(option.getPaymentPosition().getDescription());
    assertThat(responseBody.getData().getDueDate())
        .isEqualTo(DatatypeFactory.newInstance().newXMLGregorianCalendar(option.getDuoDate().toString()));
    assertThat(responseBody.getData().getRetentionDate())
        .isEqualTo(DatatypeFactory.newInstance().newXMLGregorianCalendar(option.getRetentionDate().toString()));
    assertThat(responseBody.getData().getOfficeName()).isEqualTo(option.getPaymentPosition().getOfficeName());
  }

  @Test
  void paGetPaymentwithoutRetentionDateTest() throws PartnerValidationException, DatatypeConfigurationException {

    // Test preconditions
    PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();
    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();

    option.setRetentionDate(null);

    when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
    when(factory.createCtPaymentPA()).thenReturn(factoryUtil.createCtPaymentPA());
    when(factory.createCtSubject()).thenReturn(factoryUtil.createCtSubject());
    when(factory.createCtEntityUniqueIdentifier()).thenReturn(factoryUtil.createCtEntityUniqueIdentifier());
    when(factory.createCtTransferListPA()).thenReturn(factoryUtil.createCtTransferListPA());
    when(factory.createCtTransferPA()).thenReturn(factoryUtil.createCtTransferPA());

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));

    // Test execution
    PaGetPaymentRes responseBody = partnerService.paGetPayment(requestBody);

    // Test postcondiction
    assertThat(responseBody.getData().getCreditorReferenceId()).isEqualTo(option.getNotificationCode().substring(1));
    assertThat(responseBody.getData().getRetentionDate()).isNull();
  }

  @Test
  void paGetPaymentWithoutIbanTest() throws PartnerValidationException, DatatypeConfigurationException {

    // Test preconditions
    PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();
    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();

    option.getTransfers().get(0).setIban(null);
    option.getTransfers().get(0).setPostalIban(null);

    when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
    when(factory.createCtPaymentPA()).thenReturn(factoryUtil.createCtPaymentPA());
    when(factory.createCtSubject()).thenReturn(factoryUtil.createCtSubject());
    when(factory.createCtEntityUniqueIdentifier()).thenReturn(factoryUtil.createCtEntityUniqueIdentifier());
    when(factory.createCtTransferListPA()).thenReturn(factoryUtil.createCtTransferListPA());
    when(factory.createCtTransferPA()).thenReturn(factoryUtil.createCtTransferPA());

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));

    try {
      partnerService.paGetPayment(requestBody);

    } catch (PartnerValidationException e) {

      assertTrue(true);
    }
  }

  @Test
  void paGetPaymentWithPostalIbanTest() throws PartnerValidationException, DatatypeConfigurationException {

    // Test preconditions
    PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();
    requestBody.setTransferType(StTransferType.POSTAL);

    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();

    when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
    when(factory.createCtPaymentPA()).thenReturn(factoryUtil.createCtPaymentPA());
    when(factory.createCtSubject()).thenReturn(factoryUtil.createCtSubject());
    when(factory.createCtEntityUniqueIdentifier()).thenReturn(factoryUtil.createCtEntityUniqueIdentifier());
    when(factory.createCtTransferListPA()).thenReturn(factoryUtil.createCtTransferListPA());
    when(factory.createCtTransferPA()).thenReturn(factoryUtil.createCtTransferPA());

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
        requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));

    // Test execution
    PaGetPaymentRes responseBody = partnerService.paGetPayment(requestBody);

    // Test postcondiction
    String ibanRes = responseBody.getData().getTransferList().getTransfer().get(0).getIBAN();
    String ibanTrans1 = option.getTransfers().get(0).getIban();
    String ibanTrans2 = option.getTransfers().get(1).getIban();

    assertTrue(ibanRes.equals(ibanTrans1) || ibanRes.equals(ibanTrans2));
  }

  @Test
  void paSendRTTest() throws DatatypeConfigurationException {

    // Test preconditions
    PaSendRTReq requestBody = PaSendRTReqMock.getMock();
    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();

    when(factory.createPaSendRTRes()).thenReturn(factoryUtil.createPaSendRTRes());

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getReceipt().getNoticeNumber(),
        requestBody.getReceipt().getFiscalCode())).thenReturn(Optional.of(option));

    // Test execution
    PaSendRTRes responseBody = partnerService.paSendRT(requestBody);

    // Test postcondiction
    assertThat(responseBody.getOutcome()).isEqualTo(StOutcome.OK);
  }

  @Test
  void paSendRTSaveKoTest() throws DatatypeConfigurationException {

    // Test preconditions
    PaSendRTReq requestBody = PaSendRTReqMock.getMock();
    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();

    when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getReceipt().getNoticeNumber(),
        requestBody.getReceipt().getFiscalCode())).thenReturn(Optional.of(option));

    when(paymentOptionRepository.save(any())).thenThrow(IllegalArgumentException.class);
    // Test execution
    try {

      partnerService.paSendRT(requestBody);

    } catch (IllegalArgumentException e) {

      assertTrue(true);
    }

  }

}
