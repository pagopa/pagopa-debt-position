package it.gov.pagopa.debtposition.service.pd.crud;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.TransferRepository;
import it.gov.pagopa.debtposition.model.IPaymentPositionModel;

import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import org.hibernate.exception.ConstraintViolationException;
import org.junit.jupiter.api.Test;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.util.ReflectionTestUtils;

@SpringBootTest(classes = DebtPositionApplication.class)
class PaymentPositionCRUDServiceTest {

  @Autowired private PaymentPositionCRUDService paymentsService;
  @MockitoBean private TransferRepository transferRepository;
  @MockitoBean private PaymentPositionRepository paymentPositionRepository;
  @MockitoBean private ModelMapper modelMapper;

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
  
  @Test
  void handleUniqueViolation_nonUniqueSqlState_doesNotThrowAppException() {
    ConstraintViolationException constraintViolationException =
        constraintViolation("23503", "someforeignkeyconstraint");

    assertDoesNotThrow(
        () ->
            ReflectionTestUtils.invokeMethod(
                paymentsService,
                "handleUniqueViolationAppException",
                constraintViolationException,
                "02406911202"));
  }
  
  @Test
  void handleUniqueViolation_nullConstraintName_throwsGenericDebtPositionUniqueViolation() {
    ConstraintViolationException constraintViolationException = uniqueViolation(null);

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
  
  @SuppressWarnings("unchecked")
  @Test
  void update_dataIntegrityViolationWithPaymentOptionMetadataConstraint_throwsSpecificAppException() {
    String organizationFiscalCode = "02406911202";
    String iupd = "IUPD-1";

    IPaymentPositionModel ppModel = mock(IPaymentPositionModel.class);
    when(ppModel.getIupd()).thenReturn(iupd);

    PaymentPosition ppToUpdate = new PaymentPosition();
    ppToUpdate.setIupd(iupd);
    ppToUpdate.setStatus(DebtPositionStatus.DRAFT);
    ppToUpdate.setPaymentOption(List.of());

    when(paymentPositionRepository.findOne(any(Specification.class)))
        .thenReturn(Optional.of(ppToUpdate));

    when(paymentPositionRepository.saveAndFlush(any(PaymentPosition.class)))
        .thenThrow(dataIntegrityViolation("uniquepaymentoptmetadata"));

    AppException exception =
        assertThrows(
            AppException.class,
            () ->
                paymentsService.update(
                    ppModel,
                    organizationFiscalCode,
                    false,
                    null));

    assertEquals(AppError.DEBT_POSITION_PO_METADATA_UNIQUE_VIOLATION.title, exception.getTitle());
  }
  
  @Test
  void createMultipleDebtPositions_dataIntegrityViolationWithTransferMetadataConstraint_throwsSpecificAppException() {
    String organizationFiscalCode = "02406911202";

    when(paymentPositionRepository.saveAllAndFlush(anyList()))
        .thenThrow(dataIntegrityViolation("uniquetransfermetadata"));

    List<PaymentPosition> debtPositions = List.of();

    AppException exception =
        assertThrows(
            AppException.class,
            () ->
                paymentsService.createMultipleDebtPositions(
                    debtPositions,
                    organizationFiscalCode,
                    false,
                    null));

    assertEquals(
        AppError.DEBT_POSITION_TRANSFER_METADATA_UNIQUE_VIOLATION.title,
        exception.getTitle());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  void updateMultipleDebtPositions_dataIntegrityViolationWithPaymentOptionMetadataConstraint_throwsSpecificAppException() {
    String organizationFiscalCode = "02406911202";
    String iupd = "IUPD-1";

    PaymentPositionModel inputModel = mock(PaymentPositionModel.class);
    when(inputModel.getIupd()).thenReturn(iupd);

    List<PaymentPositionModel> inputPaymentPositions = List.of(inputModel);

    PaymentPosition managedPosition = new PaymentPosition();
    managedPosition.setIupd(iupd);
    managedPosition.setStatus(DebtPositionStatus.DRAFT);
    managedPosition.setPaymentOption(List.of());

    List<PaymentPosition> managedPositions = List.of(managedPosition);

    when(paymentPositionRepository.findAll(any(Specification.class), any(Pageable.class)))
        .thenReturn(new PageImpl<>(managedPositions));

    when(paymentPositionRepository.saveAllAndFlush(anyList()))
        .thenThrow(dataIntegrityViolation("uniquepaymentoptmetadata"));

    AppException exception =
        assertThrows(
            AppException.class,
            () ->
                paymentsService.updateMultipleDebtPositions(
                    inputPaymentPositions,
                    organizationFiscalCode,
                    false,
                    null));

    assertEquals(AppError.DEBT_POSITION_PO_METADATA_UNIQUE_VIOLATION.title, exception.getTitle());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  void updateMultipleDebtPositions_dataIntegrityViolationDuringMapping_throwsSpecificAppException() {
    String organizationFiscalCode = "02406911202";
    String iupd = "IUPD-1";

    PaymentPositionModel inputModel = mock(PaymentPositionModel.class);
    when(inputModel.getIupd()).thenReturn(iupd);

    List<PaymentPositionModel> inputPaymentPositions = List.of(inputModel);

    PaymentPosition managedPosition = new PaymentPosition();
    managedPosition.setIupd(iupd);
    managedPosition.setStatus(DebtPositionStatus.DRAFT);
    managedPosition.setPaymentOption(List.of());

    List<PaymentPosition> managedPositions = List.of(managedPosition);

    when(paymentPositionRepository.findAll(any(Specification.class), any(Pageable.class)))
        .thenReturn(new PageImpl<>(managedPositions));

    doThrow(dataIntegrityViolation("uniquepaymentoptmetadata"))
    .when(modelMapper)
    .map(inputModel, managedPosition);

    AppException exception =
        assertThrows(
            AppException.class,
            () ->
                paymentsService.updateMultipleDebtPositions(
                    inputPaymentPositions,
                    organizationFiscalCode,
                    false,
                    null));

    assertEquals(AppError.DEBT_POSITION_PO_METADATA_UNIQUE_VIOLATION.title, exception.getTitle());
  }
  
  @SuppressWarnings("unchecked")
  @Test
  void update_dataIntegrityViolationDuringMapping_throwsSpecificAppException() {
    String organizationFiscalCode = "02406911202";
    String iupd = "IUPD-1";

    IPaymentPositionModel ppModel = mock(IPaymentPositionModel.class);
    when(ppModel.getIupd()).thenReturn(iupd);

    PaymentPosition ppToUpdate = new PaymentPosition();
    ppToUpdate.setIupd(iupd);
    ppToUpdate.setStatus(DebtPositionStatus.DRAFT);
    ppToUpdate.setPaymentOption(List.of());

    when(paymentPositionRepository.findOne(any(Specification.class)))
        .thenReturn(Optional.of(ppToUpdate));

    doThrow(dataIntegrityViolation("uniquepaymentoptmetadata"))
    .when(modelMapper)
    .map(ppModel, ppToUpdate);

    AppException exception =
        assertThrows(
            AppException.class,
            () ->
                paymentsService.update(
                    ppModel,
                    organizationFiscalCode,
                    false,
                    null));

    assertEquals(AppError.DEBT_POSITION_PO_METADATA_UNIQUE_VIOLATION.title, exception.getTitle());
  }

  private ConstraintViolationException uniqueViolation(String constraintName) {
    return new ConstraintViolationException(
        "duplicate key value violates unique constraint",
        new SQLException("duplicate key value violates unique constraint", "23505"),
        constraintName);
  }
  
  private ConstraintViolationException constraintViolation(String sqlState, String constraintName) {
	  return new ConstraintViolationException(
			  "database constraint violation",
			  new SQLException("database constraint violation", sqlState),
			  constraintName);
  }
  
  private DataIntegrityViolationException dataIntegrityViolation(String constraintName) {
	  return new DataIntegrityViolationException(
			  "duplicate key value violates unique constraint",
			  uniqueViolation(constraintName));
  }
}
