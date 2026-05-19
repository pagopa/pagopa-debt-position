package it.gov.pagopa.debtposition.util;

import static org.assertj.core.api.Assertions.assertThat;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;

class PublishPaymentUtilTest {

  @Test
  void publishProcess_toValid_keepsParentAtMinimumChildValidity() {
    LocalDateTime publishDatetime = LocalDateTime.of(2026, 5, 19, 10, 0);
    LocalDateTime validChild = publishDatetime.minusDays(1);
    LocalDateTime futureChild = publishDatetime.plusDays(2);
    PaymentPosition paymentPosition = buildPaymentPosition(validChild, futureChild);

    PublishPaymentUtil.publishProcess(paymentPosition, publishDatetime);

    assertThat(paymentPosition.getStatus()).isEqualTo(DebtPositionStatus.VALID);
    assertThat(paymentPosition.getValidityDate()).isEqualTo(validChild);
    assertThat(paymentPosition.getPublishDate()).isEqualTo(publishDatetime);
    assertThat(paymentPosition.getLastUpdatedDate()).isEqualTo(publishDatetime);
  }

  @Test
  void publishProcess_withNullChild_setsParentAtMinimumNormalizedValidity() {
    LocalDateTime publishDatetime = LocalDateTime.of(2026, 5, 19, 10, 0);
    LocalDateTime futureChild = publishDatetime.plusDays(2);
    PaymentPosition paymentPosition = buildPaymentPosition(null, futureChild);

    PublishPaymentUtil.publishProcess(paymentPosition, publishDatetime);

    assertThat(paymentPosition.getStatus()).isEqualTo(DebtPositionStatus.VALID);
    assertThat(paymentPosition.getPaymentOption())
        .extracting(PaymentOption::getValidityDate)
        .containsExactly(publishDatetime, futureChild);
    assertThat(paymentPosition.getValidityDate()).isEqualTo(publishDatetime);
    assertThat(paymentPosition.getPublishDate()).isEqualTo(publishDatetime);
    assertThat(paymentPosition.getLastUpdatedDate()).isEqualTo(publishDatetime);
  }

  private static PaymentPosition buildPaymentPosition(LocalDateTime... validityDates) {
    PaymentPosition paymentPosition = PaymentPosition.builder().build();

    for (int index = 0; index < validityDates.length; index++) {
      PaymentOption paymentOption =
          PaymentOption.builder()
              .id((long) index + 1)
              .validityDate(validityDates[index])
              .build();
      paymentPosition.addPaymentOption(paymentOption);
    }

    return paymentPosition;
  }
}