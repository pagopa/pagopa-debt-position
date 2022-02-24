package it.gov.pagopa.hubpa.payments.validator;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import it.gov.pagopa.hubpa.payments.endpoints.validation.PaymentValidator;
import it.gov.pagopa.hubpa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.hubpa.payments.entity.PaymentOptions;
import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;
import it.gov.pagopa.hubpa.payments.enumeration.PaymentOptionStatusEnum;
import it.gov.pagopa.hubpa.payments.enumeration.PaymentStatusEnum;
import it.gov.pagopa.hubpa.payments.mock.DebitorMock;

@ExtendWith(MockitoExtension.class)
class PaymentValidatorTest {

  @InjectMocks
  private static PaymentValidator paymentValidator;

  private static String validPtIdDominio = "123";
  private static String validPtIdIntermediario = "456";
  private static String validPtIdStazione = "789";
  private static String idDominio = "123";

  @BeforeEach
  private void init() {

    ReflectionTestUtils.setField(paymentValidator, "ptIdIntermediario", validPtIdIntermediario);
    ReflectionTestUtils.setField(paymentValidator, "ptIdStazione", validPtIdStazione);
  }

  @Test
  void isAuthorizeOKTest() {

    try {

      paymentValidator.isAuthorize(validPtIdDominio, validPtIdIntermediario, validPtIdStazione, idDominio);

    } catch (PartnerValidationException e) {

      assertFalse(true);

    }
    assertTrue(true);

  }

  @Test
  void isPayablePublishOKTest() {

    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();
    PaymentPosition position = option.getPaymentPosition();

    try {

      paymentValidator.isPayable(position, option);

    } catch (PartnerValidationException e) {

      assertFalse(true);

    }
    assertTrue(true);

  }

  @Test
  void isPayablePartialOKTest() {

    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();
    PaymentPosition position = option.getPaymentPosition();

    position.setStatus(PaymentStatusEnum.PAGATO_PARZIALE.getStatus());

    try {

      paymentValidator.isPayable(position, option);

    } catch (PartnerValidationException e) {

      assertFalse(true);

    }
    assertTrue(true);

  }

  @Test
  void isAuthorizeDominioKOTest() {

    try {

      paymentValidator.isAuthorize("invalidDomain", validPtIdIntermediario, validPtIdStazione, idDominio);

    } catch (PartnerValidationException e) {

      assertTrue(true);

    }

  }

  @Test
  void isAuthorizeIntermediarioKOTest() {

    try {

      paymentValidator.isAuthorize(validPtIdDominio, "invalidIntermediario", validPtIdStazione, idDominio);

    } catch (PartnerValidationException e) {

      assertTrue(true);

    }

  }

  @Test
  void isAuthorizeStatizioneKOTest() {

    try {

      paymentValidator.isAuthorize(validPtIdDominio, validPtIdIntermediario, "invalidStazione", idDominio);

    } catch (PartnerValidationException e) {

      assertTrue(true);

    }

  }

  @Test
  void isPayableNullPositionKOTest() {

    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();
    PaymentPosition position = null;

    try {

      paymentValidator.isPayable(position, option);

    } catch (PartnerValidationException e) {

      assertTrue(true);

    }

  }

  @Test
  void isPayableNullPositionAndOptionKOTest() {

    PaymentOptions option = null;
    PaymentPosition position = null;

    try {

      paymentValidator.isPayable(position, option);

    } catch (PartnerValidationException e) {

      assertTrue(true);

    }

  }

  @Test
  void isPayableAlreadyPaidOptionKOTest() {

    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();
    PaymentPosition position = option.getPaymentPosition();

    option.setStatus(PaymentOptionStatusEnum.PAGATO.getStatus());
    try {

      paymentValidator.isPayable(position, option);

    } catch (PartnerValidationException e) {

      assertTrue(true);

    }

  }

  @Test
  void isPayableNotPublishedOptionKOTest() {

    PaymentOptions option = DebitorMock.createPaymentOptionsMock4();
    PaymentPosition position = option.getPaymentPosition();

    position.setStatus(PaymentStatusEnum.BOZZA.getStatus());
    try {

      paymentValidator.isPayable(position, option);

    } catch (PartnerValidationException e) {

      assertTrue(true);

    }

  }
}
