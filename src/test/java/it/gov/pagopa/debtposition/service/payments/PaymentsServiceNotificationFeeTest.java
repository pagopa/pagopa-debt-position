package it.gov.pagopa.debtposition.service.payments;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import feign.FeignException;
import it.gov.pagopa.debtposition.client.NodeClient;
import it.gov.pagopa.debtposition.client.SendClient;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.model.checkposition.NodeCheckPositionModel;
import it.gov.pagopa.debtposition.model.checkposition.response.NodeCheckPositionResponse;
import it.gov.pagopa.debtposition.model.send.response.NotificationPriceResponse;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.junit.jupiter.api.extension.ExtendWith;
import org.modelmapper.ModelMapper;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
class PaymentsServiceNotificationFeeTest {

  @Mock private PaymentPositionRepository paymentPositionRepository;

  @Mock private PaymentOptionRepository paymentOptionRepository;

  @Mock private ModelMapper modelMapper;

  @Mock private NodeClient nodeClient;

  @Mock private SendClient sendClient;

  @Mock private NotificationFeeUpdateService notificationFeeUpdateService;
  
  @Mock private PaymentOptionLookupService paymentOptionLookupService;

  private PaymentsService paymentsService;

  @BeforeEach
  void setUp() {
    paymentsService =
        new PaymentsService(
            paymentPositionRepository,
            paymentOptionRepository,
            modelMapper,
            nodeClient,
            sendClient,
            notificationFeeUpdateService,
            paymentOptionLookupService);

    ReflectionTestUtils.setField(paymentsService, "auxDigit", "3");
  }

  @Test
  void updateNotificationFeeSync_OK_shouldCallSendOutsideTransactionAndDelegateUpdate() {
    PaymentOption paymentOption = new PaymentOption();
    paymentOption.setId(10L);
    paymentOption.setOrganizationFiscalCode("77777777777");
    paymentOption.setNav("312345678901234567");
    paymentOption.setAmount(1000L);
    paymentOption.setNotificationFee(0L);

    NotificationPriceResponse priceResponse =
        new NotificationPriceResponse(
            "IUN",
            1,
            1,
            0,
            0,
            ZonedDateTime.now(),
            ZonedDateTime.now(),
            1,
            1);

    PaymentOption updatedPaymentOption = new PaymentOption();
    updatedPaymentOption.setId(10L);
    updatedPaymentOption.setAmount(1001L);
    updatedPaymentOption.setNotificationFee(1L);
    updatedPaymentOption.setLastUpdatedDate(LocalDateTime.now(ZoneOffset.UTC));
    updatedPaymentOption.setLastUpdatedDateNotificationFee(LocalDateTime.now(ZoneOffset.UTC));

    when(sendClient.getNotificationFee("77777777777", "312345678901234567"))
        .thenReturn(priceResponse);
    when(notificationFeeUpdateService.applyNotificationFeeUpdate(10L, 1L))
        .thenReturn(updatedPaymentOption);

    boolean result = paymentsService.updateNotificationFeeSync(paymentOption);

    assertTrue(result);
    verify(sendClient).getNotificationFee("77777777777", "312345678901234567");
    verify(notificationFeeUpdateService).applyNotificationFeeUpdate(10L, 1L);

    org.assertj.core.api.Assertions.assertThat(paymentOption.getAmount()).isEqualTo(1001L);
    org.assertj.core.api.Assertions.assertThat(paymentOption.getNotificationFee()).isEqualTo(1L);
    org.assertj.core.api.Assertions.assertThat(paymentOption.getLastUpdatedDate()).isNotNull();
    org.assertj.core.api.Assertions
        .assertThat(paymentOption.getLastUpdatedDateNotificationFee())
        .isNotNull();
  }

  @Test
  void updateNotificationFeeSync_KO_shouldReturnFalseAndNotCallUpdateService() {
    PaymentOption paymentOption = new PaymentOption();
    paymentOption.setId(10L);
    paymentOption.setOrganizationFiscalCode("77777777777");
    paymentOption.setNav("312345678901234567");

    when(sendClient.getNotificationFee("77777777777", "312345678901234567"))
        .thenThrow(new RuntimeException("SEND timeout"));

    boolean result = paymentsService.updateNotificationFeeSync(paymentOption);

    assertFalse(result);
    verify(sendClient).getNotificationFee("77777777777", "312345678901234567");
    verify(notificationFeeUpdateService, never()).applyNotificationFeeUpdate(any(), any());
  }

  @Test
  void updateNotificationFee_OK_nodeReturnsOK_shouldSetPaymentInProgressFalse() {
    when(notificationFeeUpdateService.loadContext("77777777777", "12345678901234567"))
        .thenReturn(
            new NotificationFeeUpdateService.PaymentOptionNotificationFeeContext(
                10L,
                "77777777777",
                "312345678901234567"));

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenReturn(NodeCheckPositionResponse.builder().outcome("OK").build());

    PaymentOption updatedPaymentOption = new PaymentOption();
    updatedPaymentOption.setId(10L);
    updatedPaymentOption.setPaymentInProgress(false);

    when(notificationFeeUpdateService.applyNotificationFeeUpdate(10L, 150L, false))
        .thenReturn(updatedPaymentOption);

    paymentsService.updateNotificationFee("77777777777", "12345678901234567", 150L);

    ArgumentCaptor<NodeCheckPositionModel> captor =
        ArgumentCaptor.forClass(NodeCheckPositionModel.class);

    verify(nodeClient).getCheckPosition(captor.capture());
    verify(notificationFeeUpdateService).applyNotificationFeeUpdate(10L, 150L, false);

    org.assertj.core.api.Assertions
        .assertThat(captor.getValue().getPositionslist().get(0).getNoticeNumber())
        .isEqualTo("312345678901234567");
  }

  @Test
  void updateNotificationFee_OK_nodeReturnsKO_shouldSetPaymentInProgressTrue() {
    when(notificationFeeUpdateService.loadContext("77777777777", "12345678901234567"))
        .thenReturn(
            new NotificationFeeUpdateService.PaymentOptionNotificationFeeContext(
                10L,
                "77777777777",
                "312345678901234567"));

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenReturn(NodeCheckPositionResponse.builder().outcome("KO").build());

    PaymentOption updatedPaymentOption = new PaymentOption();
    updatedPaymentOption.setId(10L);
    updatedPaymentOption.setPaymentInProgress(true);

    when(notificationFeeUpdateService.applyNotificationFeeUpdate(10L, 150L, true))
        .thenReturn(updatedPaymentOption);

    paymentsService.updateNotificationFee("77777777777", "12345678901234567", 150L);

    verify(nodeClient).getCheckPosition(any(NodeCheckPositionModel.class));
    verify(notificationFeeUpdateService).applyNotificationFeeUpdate(10L, 150L, true);
  }

  @Test
  void updateNotificationFee_badRequestOnAuxDigitCall_shouldRetryWithNav() {
    when(notificationFeeUpdateService.loadContext("77777777777", "12345678901234567"))
        .thenReturn(
            new NotificationFeeUpdateService.PaymentOptionNotificationFeeContext(
                10L,
                "77777777777",
                "312345678901234567"));

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenThrow(FeignException.BadRequest.class)
        .thenReturn(NodeCheckPositionResponse.builder().outcome("OK").build());

    PaymentOption updatedPaymentOption = new PaymentOption();
    updatedPaymentOption.setId(10L);
    updatedPaymentOption.setPaymentInProgress(false);

    when(notificationFeeUpdateService.applyNotificationFeeUpdate(10L, 150L, false))
        .thenReturn(updatedPaymentOption);

    paymentsService.updateNotificationFee("77777777777", "12345678901234567", 150L);

    ArgumentCaptor<NodeCheckPositionModel> captor =
        ArgumentCaptor.forClass(NodeCheckPositionModel.class);

    verify(nodeClient, org.mockito.Mockito.times(2)).getCheckPosition(captor.capture());
    verify(notificationFeeUpdateService).applyNotificationFeeUpdate(10L, 150L, false);

    org.assertj.core.api.Assertions
        .assertThat(captor.getAllValues().get(0).getPositionslist().get(0).getNoticeNumber())
        .isEqualTo("312345678901234567");

    org.assertj.core.api.Assertions
        .assertThat(captor.getAllValues().get(1).getPositionslist().get(0).getNoticeNumber())
        .isEqualTo("12345678901234567");
  }

  @Test
  void updateNotificationFee_nodeException_shouldSetPaymentInProgressTrue() {
    when(notificationFeeUpdateService.loadContext("77777777777", "12345678901234567"))
        .thenReturn(
            new NotificationFeeUpdateService.PaymentOptionNotificationFeeContext(
                10L,
                "77777777777",
                "312345678901234567"));

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenThrow(new RuntimeException("Node timeout"));

    PaymentOption updatedPaymentOption = new PaymentOption();
    updatedPaymentOption.setId(10L);
    updatedPaymentOption.setPaymentInProgress(true);

    when(notificationFeeUpdateService.applyNotificationFeeUpdate(10L, 150L, true))
        .thenReturn(updatedPaymentOption);

    paymentsService.updateNotificationFee("77777777777", "12345678901234567", 150L);

    verify(nodeClient).getCheckPosition(any(NodeCheckPositionModel.class));
    verify(notificationFeeUpdateService).applyNotificationFeeUpdate(10L, 150L, true);
  }
}