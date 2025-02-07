package it.gov.pagopa.debtposition.service.pd.crud;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.repository.TransferRepository;

import java.time.LocalDateTime;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

@SpringBootTest(classes = DebtPositionApplication.class)
class PaymentPositionCRUDServiceTest {

  @Autowired private PaymentPositionCRUDService paymentsService;
  @MockBean private TransferRepository transferRepository;

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
}
