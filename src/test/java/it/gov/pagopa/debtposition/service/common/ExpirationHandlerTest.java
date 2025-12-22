package it.gov.pagopa.debtposition.service.common;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mockStatic;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Collections;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ExpirationHandlerTest {

  private PaymentPosition paymentPosition;
  private PaymentOption paymentOption;

  @BeforeEach
  void setUp() {
    // Setup base objects
    paymentPosition = new PaymentPosition();
    paymentOption = new PaymentOption();

    // Link objects
    paymentOption.setPaymentPosition(paymentPosition);
    paymentPosition.setPaymentOption(Collections.singletonList(paymentOption));
  }

  // -------------------------------------------------------------------
  // Tests for: handlePaymentPositionExpirationLogic
  // -------------------------------------------------------------------

  @Test
  void testHandlePaymentPositionExpirationLogic_ShouldExpire_WhenConditionsMet() {
    // Arrange
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentPosition.setStatus(DebtPositionStatus.VALID);
    paymentPosition.setMaxDueDate(now.minusDays(1)); // Data passata

    // Mocking UtilityMapper static call
    try (MockedStatic<UtilityMapper> utilityMapperMock = mockStatic(UtilityMapper.class)) {
      utilityMapperMock.when(() -> UtilityMapper.hasAllMarkedExpired(anyList()))
              .thenReturn(true);

      // Act
      ExpirationHandler.handlePaymentPositionExpirationLogic(paymentPosition);

      // Assert
      assertEquals(DebtPositionStatus.EXPIRED, paymentPosition.getStatus());
    }
  }

  @Test
  void testHandlePaymentPositionExpirationLogic_ShouldNotExpire_WhenStatusNotValid() {
    // Arrange
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentPosition.setStatus(DebtPositionStatus.PAID); // Non VALID
    paymentPosition.setMaxDueDate(now.minusDays(1));

    try (MockedStatic<UtilityMapper> utilityMapperMock = mockStatic(UtilityMapper.class)) {
      utilityMapperMock.when(() -> UtilityMapper.hasAllMarkedExpired(anyList()))
              .thenReturn(true);

      // Act
      ExpirationHandler.handlePaymentPositionExpirationLogic(paymentPosition);

      // Assert
      assertEquals(DebtPositionStatus.PAID, paymentPosition.getStatus());
    }
  }

  @Test
  void testHandlePaymentPositionExpirationLogic_ShouldNotExpire_WhenDateInFuture() {
    // Arrange
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentPosition.setStatus(DebtPositionStatus.VALID);
    paymentPosition.setMaxDueDate(now.plusDays(1)); // Data futura

    try (MockedStatic<UtilityMapper> utilityMapperMock = mockStatic(UtilityMapper.class)) {
      utilityMapperMock.when(() -> UtilityMapper.hasAllMarkedExpired(anyList()))
              .thenReturn(true);

      // Act
      ExpirationHandler.handlePaymentPositionExpirationLogic(paymentPosition);

      // Assert
      assertEquals(DebtPositionStatus.VALID, paymentPosition.getStatus());
    }
  }

  @Test
  void testHandlePaymentPositionExpirationLogic_ShouldNotExpire_WhenNotSwitchable() {
    // Arrange
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentPosition.setStatus(DebtPositionStatus.VALID);
    paymentPosition.setMaxDueDate(now.minusDays(1));

    try (MockedStatic<UtilityMapper> utilityMapperMock = mockStatic(UtilityMapper.class)) {
      // Return false: not all installments are configured to expire
      utilityMapperMock.when(() -> UtilityMapper.hasAllMarkedExpired(anyList()))
              .thenReturn(false);

      // Act
      ExpirationHandler.handlePaymentPositionExpirationLogic(paymentPosition);

      // Assert
      assertEquals(DebtPositionStatus.VALID, paymentPosition.getStatus());
    }
  }

  // -------------------------------------------------------------------
  // Tests for: handleInstallmentExpirationLogic
  // -------------------------------------------------------------------

  @Test
  void testHandleInstallmentExpirationLogic_ShouldExpire_WhenConditionsMet() {
    // Arrange
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);

    // 1. Parent status must be “Payable” (VALID or PARTIALLY_PAID)
    paymentPosition.setStatus(DebtPositionStatus.VALID);

    // 2. Option status must be UNPAID
    paymentOption.setStatus(PaymentOptionStatus.PO_UNPAID);

    // 3. Flag switch active
    paymentOption.setSwitchToExpired(true);

    // 4. Expired date
    paymentOption.setDueDate(now.minusDays(1));

    // Act
    ExpirationHandler.handleInstallmentExpirationLogic(now, paymentOption);

    // Assert
    assertEquals(DebtPositionStatus.EXPIRED, paymentPosition.getStatus());
  }

  @Test
  void testHandleInstallmentExpirationLogic_ShouldExpire_WhenPartiallyPaid() {
    // Arrange: PARTIALLY_PAID is considered “payable,” so it must proceed.
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentPosition.setStatus(DebtPositionStatus.PARTIALLY_PAID);

    paymentOption.setStatus(PaymentOptionStatus.PO_UNPAID);
    paymentOption.setSwitchToExpired(true);
    paymentOption.setDueDate(now.minusDays(1));

    // Act
    ExpirationHandler.handleInstallmentExpirationLogic(now, paymentOption);

    // Assert
    assertEquals(DebtPositionStatus.EXPIRED, paymentPosition.getStatus());
  }

  @Test
  void testHandleInstallmentExpirationLogic_ShouldSkip_WhenPositionStatusNotPayable() {
    // Arrange
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);

    // PAID is not payable so we expect a skip
    paymentPosition.setStatus(DebtPositionStatus.PAID);

    paymentOption.setStatus(PaymentOptionStatus.PO_UNPAID);
    paymentOption.setSwitchToExpired(true);
    paymentOption.setDueDate(now.minusDays(1));

    // Act
    ExpirationHandler.handleInstallmentExpirationLogic(now, paymentOption);

    // Assert: The state must not change
    assertEquals(DebtPositionStatus.PAID, paymentPosition.getStatus());
  }

  @Test
  void testHandleInstallmentExpirationLogic_ShouldSkip_WhenOptionNotUnpaid() {
    // Arrange
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentPosition.setStatus(DebtPositionStatus.VALID);

    paymentOption.setStatus(PaymentOptionStatus.PO_PAID); // Rata già pagata
    paymentOption.setSwitchToExpired(true);
    paymentOption.setDueDate(now.minusDays(1));

    // Act
    ExpirationHandler.handleInstallmentExpirationLogic(now, paymentOption);

    // Assert
    assertEquals(DebtPositionStatus.VALID, paymentPosition.getStatus());
  }

  @Test
  void testHandleInstallmentExpirationLogic_ShouldSkip_WhenSwitchToExpiredFalse() {
    // Arrange
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentPosition.setStatus(DebtPositionStatus.VALID);

    paymentOption.setStatus(PaymentOptionStatus.PO_UNPAID);
    paymentOption.setSwitchToExpired(false); // Flag spento
    paymentOption.setDueDate(now.minusDays(1));

    // Act
    ExpirationHandler.handleInstallmentExpirationLogic(now, paymentOption);

    // Assert
    assertEquals(DebtPositionStatus.VALID, paymentPosition.getStatus());
  }

  @Test
  void testHandleInstallmentExpirationLogic_ShouldSkip_WhenDateInFuture() {
    // Arrange
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentPosition.setStatus(DebtPositionStatus.VALID);

    paymentOption.setStatus(PaymentOptionStatus.PO_UNPAID);
    paymentOption.setSwitchToExpired(true);
    paymentOption.setDueDate(now.plusDays(1)); // Scadenza domani

    // Act
    ExpirationHandler.handleInstallmentExpirationLogic(now, paymentOption);

    // Assert
    assertEquals(DebtPositionStatus.VALID, paymentPosition.getStatus());
  }

  // -------------------------------------------------------------------
  // Tests for: isPastDueDate
  // -------------------------------------------------------------------

  @Test
  void testIsPastDueDate_True() {
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentOption.setDueDate(now.minusMinutes(1));
    assertTrue(ExpirationHandler.isPastDueDate(paymentOption, now));
  }

  @Test
  void testIsPastDueDate_False_Future() {
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentOption.setDueDate(now.plusMinutes(1));
    assertFalse(ExpirationHandler.isPastDueDate(paymentOption, now));
  }

  @Test
  void testIsPastDueDate_False_Null() {
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    paymentOption.setDueDate(null);
    assertFalse(ExpirationHandler.isPastDueDate(paymentOption, now));
  }
}