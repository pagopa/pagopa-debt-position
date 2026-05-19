package it.gov.pagopa.debtposition.util;

import static org.assertj.core.api.Assertions.assertThat;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;

class PublishPaymentUtilTest {

  private static final LocalDateTime PUBLISH_DATETIME = LocalDateTime.of(2026, 5, 19, 10, 0);

  @Test
  void publishProcess_toValid_keepsParentAtMinimumChildValidity() {
    LocalDateTime validChild = PUBLISH_DATETIME.minusDays(1);
    LocalDateTime futureChild = PUBLISH_DATETIME.plusDays(2);
    PaymentPosition paymentPosition = buildPaymentPosition(validChild, futureChild);

    PublishPaymentUtil.publishProcess(paymentPosition, PUBLISH_DATETIME);

    assertThat(paymentPosition.getStatus()).isEqualTo(DebtPositionStatus.VALID);
    assertThat(paymentPosition.getValidityDate()).isEqualTo(validChild);
    assertThat(paymentPosition.getPublishDate()).isEqualTo(PUBLISH_DATETIME);
    assertThat(paymentPosition.getLastUpdatedDate()).isEqualTo(PUBLISH_DATETIME);
  }

  @Test
  void publishProcess_withNullChild_setsParentAtMinimumNormalizedValidity() {
    LocalDateTime futureChild = PUBLISH_DATETIME.plusDays(2);
    PaymentPosition paymentPosition = buildPaymentPosition(null, futureChild);

    PublishPaymentUtil.publishProcess(paymentPosition, PUBLISH_DATETIME);

    assertThat(paymentPosition.getStatus()).isEqualTo(DebtPositionStatus.VALID);
    assertThat(paymentPosition.getPaymentOption())
        .extracting(PaymentOption::getValidityDate)
        .containsExactly(PUBLISH_DATETIME, futureChild);
    assertThat(paymentPosition.getValidityDate()).isEqualTo(PUBLISH_DATETIME);
    assertThat(paymentPosition.getPublishDate()).isEqualTo(PUBLISH_DATETIME);
    assertThat(paymentPosition.getLastUpdatedDate()).isEqualTo(PUBLISH_DATETIME);
  }

  @Test
  void publishProcess_toPublished_setsParentToEarliestFutureChildValidity() {
    LocalDateTime latestFutureChild = PUBLISH_DATETIME.plusDays(2);
    LocalDateTime earliestFutureChild = PUBLISH_DATETIME.plusDays(1);
    PaymentPosition paymentPosition = buildPaymentPosition(latestFutureChild, earliestFutureChild);

    PublishPaymentUtil.publishProcess(paymentPosition, PUBLISH_DATETIME);

    assertThat(paymentPosition.getStatus()).isEqualTo(DebtPositionStatus.PUBLISHED);
    assertThat(paymentPosition.getPaymentOption())
        .extracting(PaymentOption::getValidityDate)
        .containsExactly(latestFutureChild, earliestFutureChild);
    assertThat(paymentPosition.getValidityDate()).isEqualTo(earliestFutureChild);
    assertThat(paymentPosition.getPublishDate()).isEqualTo(PUBLISH_DATETIME);
    assertThat(paymentPosition.getLastUpdatedDate()).isEqualTo(PUBLISH_DATETIME);
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