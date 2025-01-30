package it.gov.pagopa.debtposition.service.payments;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.TransferRepository;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@SpringBootTest(classes = DebtPositionApplication.class)
class PaymentsServiceTest {

    @Autowired
    private PaymentsService paymentsService;
    @MockBean
    private PaymentPositionRepository paymentPositionRepository;
    @MockBean
    private PaymentOptionRepository paymentOptionRepository;
    @MockBean
    private TransferRepository transferRepository;

    /**
     * UPDATE IBAN ON TRANSFERS
     */

    @Test
    void updateTransferIbanMassive_OK() {
        PaymentPosition pp = PaymentPosition.builder().build();
        doReturn(List.of(pp)).when(paymentPositionRepository).findByOrganizationFiscalCodeAndStatusIn(anyString(), any());

        PaymentOption po = PaymentOption.builder().build();
        doReturn(List.of(po)).when(paymentOptionRepository).findByPaymentPositionInAndStatusIn(any(), any());

        doReturn(1).when(transferRepository).updateTransferIban(any(), anyString(), anyString(), any(LocalDateTime.class));

        int response = paymentsService.updateTransferIbanMassive("orgFiscalCode", "oldIban", "newIban");

        assertEquals(1, response);
    }

    @Test
    void updateTransferIbanMassive_KO_noPaymentPosition() {
        doReturn(List.of()).when(paymentPositionRepository).findByOrganizationFiscalCodeAndStatusIn(anyString(), any());

        verify(paymentOptionRepository, never()).findByPaymentPositionInAndStatusIn(any(), any());

        verify(transferRepository, never()).updateTransferIban(any(), anyString(), anyString(), any(LocalDateTime.class));

        assertThrows(AppException.class, () -> paymentsService.updateTransferIbanMassive("orgFiscalCode", "oldIban", "newIban"));
    }

    @Test
    void updateTransferIbanMassive_OK_noPaymentOption() {
        PaymentPosition pp = PaymentPosition.builder().build();
        doReturn(List.of(pp)).when(paymentPositionRepository).findByOrganizationFiscalCodeAndStatusIn(anyString(), any());

        doReturn(null).when(paymentOptionRepository).findByPaymentPositionInAndStatusIn(any(), any());

        verify(transferRepository, never()).updateTransferIban(any(), anyString(), anyString(), any(LocalDateTime.class));

        int response = paymentsService.updateTransferIbanMassive("orgFiscalCode", "oldIban", "newIban");

        assertEquals(0, response);
    }

    @Test
    void updateTransferIbanMassive_OK_noTransfer() {
        PaymentPosition pp = PaymentPosition.builder().build();
        doReturn(List.of(pp)).when(paymentPositionRepository).findByOrganizationFiscalCodeAndStatusIn(anyString(), any());

        PaymentOption po = PaymentOption.builder().build();
        doReturn(List.of(po)).when(paymentOptionRepository).findByPaymentPositionInAndStatusIn(any(), any());

        doReturn(0).when(transferRepository).updateTransferIban(any(), anyString(), anyString(), any(LocalDateTime.class));

        int response = paymentsService.updateTransferIbanMassive("orgFiscalCode", "oldIban", "newIban");

        assertEquals(0, response);
    }
}