package it.gov.pagopa.debtposition.service.payments;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

@ExtendWith(MockitoExtension.class)
class PaymentsServiceReportTest {

  @Mock private PaymentPositionRepository paymentPositionRepository;

  @Mock private PaymentOptionRepository paymentOptionRepository;

  @Mock private ModelMapper modelMapper;

  private PaymentsService paymentsService;

  @BeforeEach
  void setUp() {
    paymentsService =
        new PaymentsService(
            paymentPositionRepository,
            paymentOptionRepository,
            modelMapper,
            null,
            null,
            null,
            null);
  }

  @Test
  void report_withValidIur_shouldCallRepositoryWithIurAndReturnTransfer() {
    String organizationFiscalCode = "02406911202";
    String iuv = "300000000000000001";
    String transferId = "1";
    String iur = "2024001";

    PaymentPosition pp = createPaymentPosition(organizationFiscalCode, iuv, transferId, iur);

    when(paymentPositionRepository
            .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionIdReceiptAndPaymentOptionTransferIdTransfer(
                organizationFiscalCode, iuv, iur, transferId))
        .thenReturn(Optional.of(pp));

    // Mock the static validation method to avoid state check failures
    // For void methods with MockedStatic, use thenAnswer with a lambda that returns null
    try (MockedStatic<DebtPositionValidation> mockedValidation =
        mockStatic(DebtPositionValidation.class)) {
      mockedValidation
          .when(
              () ->
                  DebtPositionValidation.checkPaymentPositionAccountability(
                      any(PaymentPosition.class), any(String.class), any(String.class)))
          .thenAnswer(invocation -> null);

      Transfer result = paymentsService.report(organizationFiscalCode, iuv, transferId, iur);

      // Verify the correct repository method was called WITH iur
      verify(paymentPositionRepository)
          .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionIdReceiptAndPaymentOptionTransferIdTransfer(
              organizationFiscalCode, iuv, iur, transferId);

      org.assertj.core.api.Assertions.assertThat(result).isNotNull();
      org.assertj.core.api.Assertions.assertThat(result.getIdTransfer()).isEqualTo(transferId);
    }
  }

  @Test
  void report_withValidIur_paymentPositionNotFound_shouldThrowAppException() {
    String organizationFiscalCode = "02406911202";
    String iuv = "300000000000000001";
    String transferId = "1";
    String iur = "2024001";

    when(paymentPositionRepository
            .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionIdReceiptAndPaymentOptionTransferIdTransfer(
                organizationFiscalCode, iuv, iur, transferId))
        .thenReturn(Optional.empty());

    org.junit.jupiter.api.Assertions.assertThrows(
        AppException.class,
        () -> paymentsService.report(organizationFiscalCode, iuv, transferId, iur));

    // Verify the method WITH iur was called
    verify(paymentPositionRepository)
        .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionIdReceiptAndPaymentOptionTransferIdTransfer(
            organizationFiscalCode, iuv, iur, transferId);
  }

  @Test
  void report_withNullIur_shouldCallRepositoryWithoutIurAndReturnTransfer() {
    String organizationFiscalCode = "02406911202";
    String iuv = "300000000000000001";
    String transferId = "1";

    PaymentPosition pp = createPaymentPosition(organizationFiscalCode, iuv, transferId, null);

    when(paymentPositionRepository
            .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
                organizationFiscalCode, iuv, transferId))
        .thenReturn(Optional.of(pp));

    // Mock the static validation method to avoid state check failures
    try (MockedStatic<DebtPositionValidation> mockedValidation =
        mockStatic(DebtPositionValidation.class)) {
      mockedValidation
          .when(
              () ->
                  DebtPositionValidation.checkPaymentPositionAccountability(
                      any(PaymentPosition.class), any(String.class), any(String.class)))
          .thenAnswer(invocation -> null);

      Transfer result = paymentsService.report(organizationFiscalCode, iuv, transferId, null);

      // Verify the correct repository method was called WITHOUT iur
      verify(paymentPositionRepository)
          .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
              organizationFiscalCode, iuv, transferId);

      // Verify that the method WITH iur was NOT called
      verify(paymentPositionRepository, never())
          .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionIdReceiptAndPaymentOptionTransferIdTransfer(
              any(), any(), any(), any());

      org.assertj.core.api.Assertions.assertThat(result).isNotNull();
      org.assertj.core.api.Assertions.assertThat(result.getIdTransfer()).isEqualTo(transferId);
    }
  }

  @Test
  void report_withNullIur_paymentPositionNotFound_shouldThrowAppException() {
    String organizationFiscalCode = "02406911202";
    String iuv = "300000000000000001";
    String transferId = "1";

    when(paymentPositionRepository
            .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
                organizationFiscalCode, iuv, transferId))
        .thenReturn(Optional.empty());

    org.junit.jupiter.api.Assertions.assertThrows(
        AppException.class,
        () -> paymentsService.report(organizationFiscalCode, iuv, transferId, null));

    // Verify the method WITHOUT iur was called
    verify(paymentPositionRepository)
        .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
            organizationFiscalCode, iuv, transferId);
  }

  @Test
  void report_withBlankIur_shouldCallRepositoryWithoutIurAndReturnTransfer() {
    String organizationFiscalCode = "02406911202";
    String iuv = "300000000000000001";
    String transferId = "1";
    String blankIur = "   "; // Blank string with spaces

    PaymentPosition pp = createPaymentPosition(organizationFiscalCode, iuv, transferId, null);

    when(paymentPositionRepository
            .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
                organizationFiscalCode, iuv, transferId))
        .thenReturn(Optional.of(pp));

    // Mock the static validation method to avoid state check failures
    try (MockedStatic<DebtPositionValidation> mockedValidation =
        mockStatic(DebtPositionValidation.class)) {
      mockedValidation
          .when(
              () ->
                  DebtPositionValidation.checkPaymentPositionAccountability(
                      any(PaymentPosition.class), any(String.class), any(String.class)))
          .thenAnswer(invocation -> null);

      Transfer result = paymentsService.report(organizationFiscalCode, iuv, transferId, blankIur);

      // Verify that the method without iur was called (because isBlank() returned true)
      verify(paymentPositionRepository)
          .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
              organizationFiscalCode, iuv, transferId);

      org.assertj.core.api.Assertions.assertThat(result).isNotNull();
    }
  }

  @Test
  void report_withEmptyStringIur_shouldCallRepositoryWithoutIurAndReturnTransfer() {
    String organizationFiscalCode = "02406911202";
    String iuv = "300000000000000001";
    String transferId = "1";
    String emptyIur = ""; // Empty string

    PaymentPosition pp = createPaymentPosition(organizationFiscalCode, iuv, transferId, null);

    when(paymentPositionRepository
            .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
                organizationFiscalCode, iuv, transferId))
        .thenReturn(Optional.of(pp));

    try (MockedStatic<DebtPositionValidation> mockedValidation =
        mockStatic(DebtPositionValidation.class)) {
      mockedValidation
          .when(
              () ->
                  DebtPositionValidation.checkPaymentPositionAccountability(
                      any(PaymentPosition.class), any(String.class), any(String.class)))
          .thenAnswer(invocation -> null);

      Transfer result = paymentsService.report(organizationFiscalCode, iuv, transferId, emptyIur);

      verify(paymentPositionRepository)
          .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
              organizationFiscalCode, iuv, transferId);

      org.assertj.core.api.Assertions.assertThat(result).isNotNull();
    }
  }

  private PaymentPosition createPaymentPosition(
      String organizationFiscalCode, String iuv, String transferId, String iur) {

    PaymentPosition pp = new PaymentPosition();
    pp.setOrganizationFiscalCode(organizationFiscalCode);
    pp.setIupd("IUPD-001");

    Transfer transfer = new Transfer();
    transfer.setIdTransfer(transferId);
    transfer.setOrganizationFiscalCode(organizationFiscalCode);
    transfer.setStatus(TransferStatus.T_UNREPORTED);
    transfer.setAmount(100L);
    transfer.setLastUpdatedDate(java.time.LocalDateTime.now());

    PaymentOption po = new PaymentOption();
    po.setIuv(iuv);
    po.setIdReceipt(iur);
    po.setOrganizationFiscalCode(organizationFiscalCode);
    po.setTransfer(List.of(transfer));
    po.setStatus(PaymentOptionStatus.PO_UNPAID);
    po.setLastUpdatedDate(java.time.LocalDateTime.now());

    pp.setPaymentOption(List.of(po));

    return pp;
  }
}
