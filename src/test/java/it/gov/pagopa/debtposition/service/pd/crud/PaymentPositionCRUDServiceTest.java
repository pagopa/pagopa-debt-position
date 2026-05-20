package it.gov.pagopa.debtposition.service.pd.crud;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.repository.TransferRepository;

import java.sql.SQLException;
import java.time.LocalDateTime;

import org.hibernate.exception.ConstraintViolationException;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.util.ReflectionTestUtils;

@SpringBootTest(classes = DebtPositionApplication.class)
class PaymentPositionCRUDServiceTest {

  @Autowired private PaymentPositionCRUDService paymentsService;
  @MockitoBean private TransferRepository transferRepository;

  /** UPDATE IBAN ON TRANSFERS */
  @Test
  void updateTransferIbanMassive_OK() {
    doReturn(1)
        .when(transferRepository)
            .updateTransferIban(any(), anyString(), anyString(), any(LocalDateTime.class), anyList(), anyList(), anyInt());

    int response = paymentsService.updateTransferIbanMassive("orgFiscalCode", "oldIban", "newIban", 10);

    assertEquals(1, response);
  }

  @Test
  void updateTransferIbanMassive_OK_noTransfer() {
    doReturn(0)
        .when(transferRepository)
        .updateTransferIban(any(), anyString(), anyString(), any(LocalDateTime.class), anyList(), anyList(), anyInt());

    int response = paymentsService.updateTransferIbanMassive("orgFiscalCode", "oldIban", "newIban", 10);

    assertEquals(0, response);
  }
  
  @Test
  void handleUniqueViolation_paymentOptionMetadataConstraint_throwsSpecificAppException() {
    ConstraintViolationException constraintViolationException =
        uniqueViolation("uniquepaymentoptmetadata");

    AppException exception =
        assertThrows(
            AppException.class,
            () ->
                ReflectionTestUtils.invokeMethod(
                    paymentsService,
                    "handleUniqueViolationAppException",
                    constraintViolationException,
                    "02406911202"));

    assertEquals(AppError.DEBT_POSITION_PO_METADATA_UNIQUE_VIOLATION.title, exception.getTitle());
  }

  @Test
  void handleUniqueViolation_transferMetadataConstraint_throwsSpecificAppException() {
    ConstraintViolationException constraintViolationException =
        uniqueViolation("uniquetransfermetadata");

    AppException exception =
        assertThrows(
            AppException.class,
            () ->
                ReflectionTestUtils.invokeMethod(
                    paymentsService,
                    "handleUniqueViolationAppException",
                    constraintViolationException,
                    "02406911202"));

    assertEquals(AppError.DEBT_POSITION_TRANSFER_METADATA_UNIQUE_VIOLATION.title, exception.getTitle());
  }

  @Test
  void handleUniqueViolation_unknownUniqueConstraint_throwsGenericDebtPositionUniqueViolation() {
    ConstraintViolationException constraintViolationException =
        uniqueViolation("someotheruniqueconstraint");

    AppException exception =
        assertThrows(
            AppException.class,
            () ->
                ReflectionTestUtils.invokeMethod(
                    paymentsService,
                    "handleUniqueViolationAppException",
                    constraintViolationException,
                    "02406911202"));

    assertEquals(AppError.DEBT_POSITION_UNIQUE_VIOLATION.title, exception.getTitle());
  }

  private ConstraintViolationException uniqueViolation(String constraintName) {
    return new ConstraintViolationException(
        "duplicate key value violates unique constraint",
        new SQLException("duplicate key value violates unique constraint", "23505"),
        constraintName);
  }
}
