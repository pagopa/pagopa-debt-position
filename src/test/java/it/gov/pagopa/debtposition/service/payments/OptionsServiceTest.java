package it.gov.pagopa.debtposition.service.payments;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.payments.verify.response.PaymentOptionGroup;
import it.gov.pagopa.debtposition.model.payments.verify.response.VerifyPaymentOptionsResponse;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class OptionsServiceTest {

  @Mock private PaymentOptionRepository paymentOptionRepository;

  @InjectMocks private OptionsService optionsService;

  private PaymentPosition mockPosition;
  private PaymentOption mockOptionSingle;
  private final String fiscalCode = "12345678901";
  private final String noticeNumber = "302000100000009483";

  @BeforeEach
  void setUp() {
    // Basic setup valid for most tests
    mockPosition = new PaymentPosition();
    mockPosition.setId(100L);
    mockPosition.setOrganizationFiscalCode(fiscalCode);
    mockPosition.setCompanyName("Company SpA");
    mockPosition.setStatus(DebtPositionStatus.PUBLISHED); // Let's start with Published to test the transitions.

    mockOptionSingle = new PaymentOption();
    mockOptionSingle.setId(1L);
    mockOptionSingle.setNav(noticeNumber);
    mockOptionSingle.setAmount(1000L);
    mockOptionSingle.setIsPartialPayment(false); // Single Payment
    mockOptionSingle.setStatus(PaymentOptionStatus.PO_UNPAID);
    mockOptionSingle.setPaymentPosition(mockPosition);
    mockOptionSingle.setTransfer(Collections.emptyList()); // Evita NPE su allCCP check

    mockPosition.setPaymentOption(new ArrayList<>(Collections.singletonList(mockOptionSingle)));
  }

  @Test
  void verifyPaymentOptions_HappyPath_SingleOption() {
    // Arrange
    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            anyString(), anyString(), anyString(), anyString()))
            .thenReturn(Optional.of(mockOptionSingle));

    // Act
    VerifyPaymentOptionsResponse response = optionsService.verifyPaymentOptions(fiscalCode, noticeNumber);

    // Assert
    assertNotNull(response);
    assertEquals(fiscalCode, response.getOrganizationFiscalCode());
    assertEquals(1, response.getPaymentOptions().size());

    PaymentOptionGroup group = response.getPaymentOptions().get(0);
    assertEquals("PO_UNPAID", group.getStatus());
    assertEquals(1000L, group.getAmount());
    assertFalse(group.getInstallments().isEmpty());
  }

  @Test
  void verifyPaymentOptions_ShouldThrow_WhenNotFound() {
    // Arrange
    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            anyString(), anyString(), anyString(), anyString()))
            .thenReturn(Optional.empty());

    // Act & Assert
    AppException exception =
            assertThrows(
                    AppException.class, () -> optionsService.verifyPaymentOptions(fiscalCode, noticeNumber));
    assertEquals("Not found the payment option", exception.getTitle());
  }

  @Test
  void verifyPaymentOptions_ShouldReturnInvalid_WhenPositionIsInvalid() {
    // Arrange
    mockPosition.setStatus(DebtPositionStatus.INVALID); // Canceled Position
    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            anyString(), anyString(), anyString(), anyString()))
            .thenReturn(Optional.of(mockOptionSingle));

    // Act
    VerifyPaymentOptionsResponse response = optionsService.verifyPaymentOptions(fiscalCode, noticeNumber);

    // Assert
    PaymentOptionGroup group = response.getPaymentOptions().get(0);
    assertEquals("PO_INVALID", group.getStatus());
    assertEquals("Debt position is INVALID or EXPIRED", group.getStatusReason());
  }

  @Test
  void verifyPaymentOptions_ShouldMarkExpiredNotPayable_WhenDueDatePassedAndSwitchTrue() {
    // Arrange
    LocalDateTime pastDate = LocalDateTime.now(ZoneOffset.UTC).minusDays(10);
    mockPosition.setStatus(DebtPositionStatus.VALID);
    mockPosition.setMaxDueDate(pastDate);

    mockOptionSingle.setDueDate(pastDate);
    mockOptionSingle.setSwitchToExpired(true);

    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            anyString(), anyString(), anyString(), anyString()))
            .thenReturn(Optional.of(mockOptionSingle));

    // Act
    VerifyPaymentOptionsResponse response = optionsService.verifyPaymentOptions(fiscalCode, noticeNumber);

    // Assert
    PaymentOptionGroup group = response.getPaymentOptions().get(0);
    // Could be PO_EXPIRED_NOT_PAYABLE instead of PO_INVALID?
    assertEquals("PO_INVALID", group.getStatus());
    // The position should be transitioned to EXPIRED
    assertEquals(DebtPositionStatus.EXPIRED, mockPosition.getStatus());
  }

  @Test
  void verifyPaymentOptions_ShouldMarkExpiredButPayable_WhenDueDatePassedAndSwitchFalse() {
    // Arrange
    LocalDateTime pastDate = LocalDateTime.now(ZoneOffset.UTC).minusDays(10);
    mockPosition.setStatus(DebtPositionStatus.VALID);

    mockOptionSingle.setDueDate(pastDate);
    mockOptionSingle.setSwitchToExpired(false); // Expires but can be paid

    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            anyString(), anyString(), anyString(), anyString()))
            .thenReturn(Optional.of(mockOptionSingle));

    // Act
    VerifyPaymentOptionsResponse response = optionsService.verifyPaymentOptions(fiscalCode, noticeNumber);

    // Assert
    PaymentOptionGroup group = response.getPaymentOptions().get(0);
    assertEquals("PO_EXPIRED_UNPAID", group.getStatus());
  }

  @Test
  void verifyPaymentOptions_ActivePlanLogic_ShouldInvalidateSingle_WhenInstallmentPaid() {
    // Arrange: Let's create a scenario with 1 Single Option and 2 Installments
    PaymentOption optionFull = createOption(2L, "FULL", false, PaymentOptionStatus.PO_UNPAID, null);
    PaymentOption install1 = createOption(3L, "PLAN_A", true, PaymentOptionStatus.PO_PAID, "PLAN_1"); // PAGATA
    PaymentOption install2 = createOption(4L, "PLAN_A", true, PaymentOptionStatus.PO_UNPAID, "PLAN_1");

    // Add all to position
    mockPosition.setPaymentOption(Arrays.asList(optionFull, install1, install2));
    mockPosition.setStatus(DebtPositionStatus.PARTIALLY_PAID);

    // The user searches for the unpaid installment (installment 2)
    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            anyString(), anyString(), anyString(), anyString()))
            .thenReturn(Optional.of(install2));

    // Act
    VerifyPaymentOptionsResponse response = optionsService.verifyPaymentOptions(fiscalCode, noticeNumber);

    // Assert
    // We expect two groups: one for FULL (Single) and one for PLAN_
    List<PaymentOptionGroup> groups = response.getPaymentOptions();
    assertEquals(2, groups.size());

    // 1. Check Group “FULL” (Single) -> Must be PO_INVALID because there is an active plan
    PaymentOptionGroup fullGroup = groups.stream()
            .filter(g -> g.getNumberOfInstallments() == 1)
            .findFirst().orElseThrow();
    assertEquals("PO_INVALID", fullGroup.getStatus());
    assertTrue(fullGroup.getStatusReason().contains("another payment option has already been used"));

    // 2. Check “PLAN” Group -> Must be PO_PARTIALLY_PAID (one installment paid, one unpaid)
    PaymentOptionGroup planGroup = groups.stream()
            .filter(g -> g.getNumberOfInstallments() == 2)
            .findFirst().orElseThrow();
    assertEquals("PO_PARTIALLY_PAID", planGroup.getStatus());
  }

  @Test
  void verifyPaymentOptions_AllCCPCheck() {
    // Arrange
    mockOptionSingle.setTransfer(Collections.singletonList(createTransfer("IT0000000000000000000000001")));
    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            anyString(), anyString(), anyString(), anyString()))
            .thenReturn(Optional.of(mockOptionSingle));

    // Act
    VerifyPaymentOptionsResponse response = optionsService.verifyPaymentOptions(fiscalCode, noticeNumber);

    // Assert
    assertTrue(response.getPaymentOptions().get(0).getAllCCP());
  }

  @Test
  void verifyPaymentOptions_AllCCPCheck_FalseIfMissing() {
    // Arrange
    mockOptionSingle.setTransfer(Collections.singletonList(createTransfer(null))); // IBAN Mancante
    when(paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            anyString(), anyString(), anyString(), anyString()))
            .thenReturn(Optional.of(mockOptionSingle));

    // Act
    VerifyPaymentOptionsResponse response = optionsService.verifyPaymentOptions(fiscalCode, noticeNumber);

    // Assert
    assertFalse(response.getPaymentOptions().get(0).getAllCCP());
  }

  // --- Helper Methods ---

  private PaymentOption createOption(Long id, String nav, boolean isPartial, PaymentOptionStatus status, String planId) {
    PaymentOption po = new PaymentOption();
    po.setId(id);
    po.setNav(nav);
    po.setAmount(100L);
    po.setIsPartialPayment(isPartial);
    po.setStatus(status);
    po.setPaymentPlanId(planId);
    po.setPaymentPosition(mockPosition);
    po.setTransfer(Collections.emptyList());
    return po;
  }

  private Transfer createTransfer(String iban) {
    Transfer t = new Transfer();
    t.setPostalIban(iban);
    return t;
  }
}