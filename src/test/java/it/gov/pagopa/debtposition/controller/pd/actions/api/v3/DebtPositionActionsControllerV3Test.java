package it.gov.pagopa.debtposition.controller.pd.actions.api.v3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import it.gov.pagopa.debtposition.service.pd.actions.PaymentPositionActionsService;
import org.junit.jupiter.api.Test;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

class DebtPositionActionsControllerV3Test {

  @Test
  void publishDebtPosition_returnsOk_whenServiceReturnsPosition() {
    ModelMapper modelMapper = new ModelMapper();
    PaymentPositionActionsService service = mock(PaymentPositionActionsService.class);
    DebtPositionActionsControllerV3 controller =
        new DebtPositionActionsControllerV3(modelMapper, service);

    PaymentPosition paymentPosition = new PaymentPosition();
    paymentPosition.setIupd("IUPD-1");
    when(service.publish("ORG", "IUPD-1", null)).thenReturn(paymentPosition);

    ResponseEntity<PaymentPositionModelV3> response =
        controller.publishDebtPosition("ORG", "IUPD-1", null);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertNotNull(response.getBody());
    assertEquals("IUPD-1", response.getBody().getIupd());
  }

  @Test
  void publishDebtPosition_throwsException_whenServiceReturnsNull() {
    ModelMapper modelMapper = new ModelMapper();
    PaymentPositionActionsService service = mock(PaymentPositionActionsService.class);
    DebtPositionActionsControllerV3 controller =
        new DebtPositionActionsControllerV3(modelMapper, service);

    when(service.publish("ORG", "IUPD-1", null)).thenReturn(null);

    AppException exception =
        assertThrows(
            AppException.class,
            () -> controller.publishDebtPosition("ORG", "IUPD-1", null));

    assertEquals(AppError.DEBT_POSITION_PUBLISH_FAILED, exception.getAppError());
  }

  @Test
  void invalidateDebtPosition_returnsOk_whenServiceReturnsPosition() {
    ModelMapper modelMapper = new ModelMapper();
    PaymentPositionActionsService service = mock(PaymentPositionActionsService.class);
    DebtPositionActionsControllerV3 controller =
        new DebtPositionActionsControllerV3(modelMapper, service);

    PaymentPosition paymentPosition = new PaymentPosition();
    paymentPosition.setIupd("IUPD-1");
    when(service.invalidate("ORG", "IUPD-1", null, ServiceType.GPD))
        .thenReturn(paymentPosition);

    ResponseEntity<PaymentPositionModelV3> response =
        controller.invalidateDebtPosition("ORG", "IUPD-1", null, ServiceType.GPD);

    assertEquals(HttpStatus.OK, response.getStatusCode());
    assertNotNull(response.getBody());
    assertEquals("IUPD-1", response.getBody().getIupd());
  }

  @Test
  void invalidateDebtPosition_throwsException_whenServiceReturnsNull() {
    ModelMapper modelMapper = new ModelMapper();
    PaymentPositionActionsService service = mock(PaymentPositionActionsService.class);
    DebtPositionActionsControllerV3 controller =
        new DebtPositionActionsControllerV3(modelMapper, service);

    when(service.invalidate("ORG", "IUPD-1", null, ServiceType.GPD)).thenReturn(null);

    AppException exception =
        assertThrows(
            AppException.class,
            () -> controller.invalidateDebtPosition("ORG", "IUPD-1", null, ServiceType.GPD));

    assertEquals(AppError.DEBT_POSITION_INVALIDATE_FAILED, exception.getAppError());
  }
}

