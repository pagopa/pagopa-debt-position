package it.gov.pagopa.debtposition.service.common;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ValidityHandlerTest {

  private static final LocalDateTime REFERENCE_DATETIME = LocalDateTime.of(2026, 5, 19, 10, 0);

  @Test
  void handlePaymentPositionValidTransition_shouldSetValid_whenCurrentDateEqualsMinValidity() {
    PaymentPosition paymentPosition = buildPublishedPosition(REFERENCE_DATETIME);

    try (MockedStatic<LocalDateTime> localDateTimeMock =
            mockStatic(LocalDateTime.class, CALLS_REAL_METHODS)) {
      localDateTimeMock.when(() -> LocalDateTime.now(ZoneOffset.UTC)).thenReturn(REFERENCE_DATETIME);

      ValidityHandler.handlePaymentPositionValidTransition(paymentPosition);

      assertThat(paymentPosition.getStatus()).isEqualTo(DebtPositionStatus.VALID);
    }
  }

  @Test
  void isInstallmentValid_shouldReturnTrue_whenCurrentDateEqualsValidityDate() {
    PaymentOption paymentOption = new PaymentOption();
    paymentOption.setValidityDate(REFERENCE_DATETIME);

    assertThat(ValidityHandler.isInstallmentValid(REFERENCE_DATETIME, paymentOption)).isTrue();
  }

  private static PaymentPosition buildPublishedPosition(LocalDateTime... validityDates) {
    PaymentPosition paymentPosition = new PaymentPosition();
    paymentPosition.setStatus(DebtPositionStatus.PUBLISHED);

    for (LocalDateTime validityDate : validityDates) {
      PaymentOption paymentOption = new PaymentOption();
      paymentOption.setValidityDate(validityDate);
      paymentPosition.addPaymentOption(paymentOption);
    }

    return paymentPosition;
  }
}