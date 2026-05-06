package it.gov.pagopa.debtposition.service.payments;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.service.payments.PaymentsService.PaymentOptionNotificationFeeContext;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
class NotificationFeeUpdateServiceTest {

  @Mock private PaymentOptionRepository paymentOptionRepository;

  private NotificationFeeUpdateService notificationFeeUpdateService;

  @BeforeEach
  void setUp() {
    notificationFeeUpdateService = new NotificationFeeUpdateService(paymentOptionRepository);
  }

  @Test
  void loadContext_OK_shouldReturnMinimalContext() {
    PaymentOption paymentOption = new PaymentOption();
    paymentOption.setId(10L);
    paymentOption.setOrganizationFiscalCode("77777777777");
    paymentOption.setNav("312345678901234567");
    paymentOption.setStatus(PaymentOptionStatus.PO_UNPAID);

    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            "77777777777",
            "12345678901234567",
            "77777777777",
            "12345678901234567"))
        .thenReturn(Optional.of(paymentOption));

    PaymentOptionNotificationFeeContext context =
        notificationFeeUpdateService.loadContext("77777777777", "12345678901234567");

    org.assertj.core.api.Assertions.assertThat(context.paymentOptionId()).isEqualTo(10L);
    org.assertj.core.api.Assertions.assertThat(context.organizationFiscalCode()).isEqualTo("77777777777");
    org.assertj.core.api.Assertions.assertThat(context.nav()).isEqualTo("312345678901234567");
  }

  @Test
  void loadContext_notFound_shouldThrowAppException() {
    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            "77777777777",
            "12345678901234567",
            "77777777777",
            "12345678901234567"))
        .thenReturn(Optional.empty());

    assertThrows(
        AppException.class,
        () -> notificationFeeUpdateService.loadContext("77777777777", "12345678901234567"));
  }

  @Test
  void loadContext_paidPaymentOption_shouldThrowAppException() {
    PaymentOption paymentOption = new PaymentOption();
    paymentOption.setId(10L);
    paymentOption.setOrganizationFiscalCode("77777777777");
    paymentOption.setNav("312345678901234567");
    paymentOption.setStatus(PaymentOptionStatus.PO_PAID);

    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            "77777777777",
            "12345678901234567",
            "77777777777",
            "12345678901234567"))
        .thenReturn(Optional.of(paymentOption));

    assertThrows(
        AppException.class,
        () -> notificationFeeUpdateService.loadContext("77777777777", "12345678901234567"));
  }

  @Test
  void applyNotificationFeeUpdate_OK_shouldSubtractOldFeeAndApplyNewFee() {
    PaymentOption paymentOption = buildPaymentOptionWithTransfer();
    paymentOption.setAmount(1100L);
    paymentOption.setNotificationFee(100L);

    Transfer transfer = paymentOption.getTransfer().get(0);
    transfer.setAmount(1100L);

    when(paymentOptionRepository.findById(10L)).thenReturn(Optional.of(paymentOption));
    when(paymentOptionRepository.saveAndFlush(any(PaymentOption.class)))
        .thenAnswer(invocation -> invocation.getArgument(0));

    PaymentOption result =
        notificationFeeUpdateService.applyNotificationFeeUpdate(10L, 150L, false);

    org.assertj.core.api.Assertions.assertThat(result.getNotificationFee()).isEqualTo(150L);
    org.assertj.core.api.Assertions.assertThat(result.getAmount()).isEqualTo(1150L);
    org.assertj.core.api.Assertions.assertThat(result.getTransfer().get(0).getAmount()).isEqualTo(1150L);
    org.assertj.core.api.Assertions.assertThat(result.getLastUpdatedDate()).isNotNull();
    org.assertj.core.api.Assertions.assertThat(result.getLastUpdatedDateNotificationFee()).isNotNull();

    verify(paymentOptionRepository).saveAndFlush(paymentOption);
  }

  @Test
  void applyNotificationFeeUpdate_withoutPaymentInProgress_OK_shouldUpdateAmountsAndDates() {
    PaymentOption paymentOption = buildPaymentOptionWithTransfer();
    paymentOption.setAmount(1000L);
    paymentOption.setNotificationFee(0L);

    Transfer transfer = paymentOption.getTransfer().get(0);
    transfer.setAmount(1000L);

    when(paymentOptionRepository.findById(10L)).thenReturn(Optional.of(paymentOption));
    when(paymentOptionRepository.saveAndFlush(any(PaymentOption.class)))
        .thenAnswer(invocation -> invocation.getArgument(0));

    PaymentOption result =
        notificationFeeUpdateService.applyNotificationFeeUpdate(10L, 150L);

    org.assertj.core.api.Assertions.assertThat(result.getNotificationFee()).isEqualTo(150L);
    org.assertj.core.api.Assertions.assertThat(result.getAmount()).isEqualTo(1150L);
    org.assertj.core.api.Assertions.assertThat(result.getTransfer().get(0).getAmount()).isEqualTo(1150L);
    org.assertj.core.api.Assertions.assertThat(result.getLastUpdatedDate()).isNotNull();
    org.assertj.core.api.Assertions.assertThat(result.getLastUpdatedDateNotificationFee()).isNotNull();

    verify(paymentOptionRepository).saveAndFlush(paymentOption);
  }

  @Test
  void applyNotificationFeeUpdate_notFound_shouldThrowAppException() {
    when(paymentOptionRepository.findById(10L)).thenReturn(Optional.empty());

    assertThrows(
        AppException.class,
        () -> notificationFeeUpdateService.applyNotificationFeeUpdate(10L, 150L, false));

    verify(paymentOptionRepository, never()).saveAndFlush(any(PaymentOption.class));
  }

  @Test
  void applyNotificationFeeUpdate_noValidTransfer_shouldThrowAppExceptionAndNotSave() {
    PaymentOption paymentOption = new PaymentOption();
    paymentOption.setId(10L);
    paymentOption.setIuv("1234561");
    paymentOption.setOrganizationFiscalCode("77777777777");
    paymentOption.setAmount(1000L);
    paymentOption.setNotificationFee(0L);

    Transfer transfer = new Transfer();
    transfer.setIdTransfer("1");
    transfer.setOrganizationFiscalCode("99999999999");
    transfer.setAmount(1000L);

    paymentOption.addTransfer(transfer);

    when(paymentOptionRepository.findById(10L)).thenReturn(Optional.of(paymentOption));

    assertThrows(
        AppException.class,
        () -> notificationFeeUpdateService.applyNotificationFeeUpdate(10L, 150L, false));

    verify(paymentOptionRepository, never()).saveAndFlush(any(PaymentOption.class));
  }

  private PaymentOption buildPaymentOptionWithTransfer() {
    PaymentOption paymentOption = new PaymentOption();
    paymentOption.setId(10L);
    paymentOption.setIuv("1234561");
    paymentOption.setNav("31234561");
    paymentOption.setOrganizationFiscalCode("77777777777");
    paymentOption.setStatus(PaymentOptionStatus.PO_UNPAID);

    Transfer transfer = new Transfer();
    transfer.setIdTransfer("1");
    transfer.setOrganizationFiscalCode("77777777777");

    paymentOption.addTransfer(transfer);

    return paymentOption;
  }
}