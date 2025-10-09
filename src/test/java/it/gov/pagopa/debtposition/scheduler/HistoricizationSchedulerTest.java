package it.gov.pagopa.debtposition.scheduler;

import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.azure.data.tables.TableClient;
import com.azure.data.tables.models.TableTransactionFailedException;
import com.azure.data.tables.models.TableTransactionResult;
import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.config.SchedulerConfig;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import jakarta.persistence.EntityManager;
import jakarta.persistence.TypedQuery;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.util.ReflectionTestUtils;

@SpringBootTest(classes = DebtPositionApplication.class)
@SpringJUnitConfig(SchedulerConfig.class)
class HistoricizationSchedulerTest {

  @MockitoBean PaymentPositionRepository paymentPositionRepository;

  // azure storage params
  @Value("${azure.archive.storage.connection}")
  private String archiveStorageConnection;

  @Test
  void manualTotalEntriesHistoricization() throws Exception {

    HistoricizationScheduler scheduler =
        spy(new HistoricizationScheduler(paymentPositionRepository));

    ReflectionTestUtils.setField(scheduler, "paginationMode", false);

    // precondition
    EntityManager entityManager = mock(EntityManager.class);
    doReturn(entityManager).when(scheduler).getEntityManager();

    TypedQuery<PaymentPosition> mockedQuery = mock(TypedQuery.class);
    when(entityManager.createQuery(any(), eq(PaymentPosition.class))).thenReturn(mockedQuery);
    when(mockedQuery.setParameter(anyInt(), any())).thenReturn(mockedQuery);

    List<PaymentPosition> expected = new ArrayList<>();
    PaymentOption po =
        PaymentOption.builder().iuv("mockIuv").paymentDate(LocalDateTime.now()).build();
    PaymentPosition pp =
        PaymentPosition.builder()
            .organizationFiscalCode("77777777777")
            .iupd("mockIupd")
            .paymentOption(List.of(po))
            .build();
    expected.add(pp);
    when(mockedQuery.getResultList()).thenReturn(expected);

    TableClient tc = mock(TableClient.class);
    doReturn(tc).when(scheduler).getTableClient(any(), any());
    doReturn(mock(TableTransactionResult.class)).when(tc).submitTransaction(any());

    // lancio il batch di archiviazione delle posizioni debitorie
    scheduler.manageDebtPositionsToHistoricize();

    verify(scheduler, times(1)).upsertPPTable(any(), any());
    verify(paymentPositionRepository, times(1)).deleteAll(any());
  }

  @Test
  void manualPaginatedEntriesHistoricization() throws Exception {

    HistoricizationScheduler scheduler =
        spy(new HistoricizationScheduler(paymentPositionRepository));

    ReflectionTestUtils.setField(scheduler, "paginationMode", true);
    ReflectionTestUtils.setField(scheduler, "pageSize", 5);
    ReflectionTestUtils.setField(scheduler, "maxBatchOperationSize", (short) 1);

    // precondition
    EntityManager entityManager = mock(EntityManager.class);
    doReturn(entityManager).when(scheduler).getEntityManager();

    TypedQuery<Long> mockedCountQuery = mock(TypedQuery.class);
    TypedQuery<PaymentPosition> mockedQuery = mock(TypedQuery.class);
    when(entityManager.createQuery(any(), eq(Long.class))).thenReturn(mockedCountQuery);
    when(entityManager.createQuery(any(), eq(PaymentPosition.class))).thenReturn(mockedQuery);
    when(mockedQuery.setParameter(anyInt(), any())).thenReturn(mockedQuery);
    when(mockedQuery.setFirstResult(anyInt())).thenReturn(mockedQuery);
    when(mockedQuery.setMaxResults(anyInt())).thenReturn(mockedQuery);
    when(mockedCountQuery.setParameter(anyInt(), any())).thenReturn(mockedCountQuery);

    List<PaymentPosition> expected = new ArrayList<>();
    PaymentOption po =
        PaymentOption.builder().iuv("mockIuv").paymentDate(LocalDateTime.now()).build();
    PaymentPosition pp1 =
        PaymentPosition.builder()
            .organizationFiscalCode("77777777777")
            .iupd("mockIupd")
            .paymentOption(List.of(po))
            .build();
    PaymentPosition pp2 =
        PaymentPosition.builder()
            .organizationFiscalCode("77777777777")
            .iupd("mockIupd")
            .paymentOption(List.of(po))
            .build();
    PaymentPosition pp3 =
        PaymentPosition.builder()
            .organizationFiscalCode("77777777777")
            .iupd("mockIupd")
            .paymentOption(List.of(po))
            .build();
    expected.add(pp1);
    expected.add(pp2);
    expected.add(pp3);
    when(mockedQuery.getResultList()).thenReturn(expected);
    when(mockedCountQuery.getSingleResult()).thenReturn(3L);

    TableClient tc = mock(TableClient.class);
    doReturn(tc).when(scheduler).getTableClient(any(), any());
    doReturn(mock(TableTransactionResult.class)).when(tc).submitTransaction(any());

    // lancio il batch di archiviazione delle posizioni debitorie
    scheduler.manageDebtPositionsToHistoricize();

    verify(scheduler, times(1)).upsertPPTable(any(), any());
    verify(paymentPositionRepository, times(1)).deleteAll(any());
  }

  @Test
  void manualKOHistoricization() throws Exception {

    HistoricizationScheduler scheduler =
        spy(new HistoricizationScheduler(paymentPositionRepository));

    ReflectionTestUtils.setField(scheduler, "paginationMode", true);
    ReflectionTestUtils.setField(scheduler, "pageSize", 5);

    // precondition
    EntityManager entityManager = mock(EntityManager.class);
    doReturn(entityManager).when(scheduler).getEntityManager();

    TypedQuery<Long> mockedCountQuery = mock(TypedQuery.class);
    TypedQuery<PaymentPosition> mockedQuery = mock(TypedQuery.class);
    when(entityManager.createQuery(any(), eq(Long.class))).thenReturn(mockedCountQuery);
    when(entityManager.createQuery(any(), eq(PaymentPosition.class))).thenReturn(mockedQuery);
    when(mockedQuery.setParameter(anyInt(), any())).thenReturn(mockedQuery);
    when(mockedQuery.setFirstResult(anyInt())).thenReturn(mockedQuery);
    when(mockedQuery.setMaxResults(anyInt())).thenReturn(mockedQuery);
    when(mockedCountQuery.setParameter(anyInt(), any())).thenReturn(mockedCountQuery);

    List<PaymentPosition> expected = new ArrayList<>();
    PaymentOption po =
        PaymentOption.builder().iuv("mockIuv").paymentDate(LocalDateTime.now()).build();
    PaymentPosition pp =
        PaymentPosition.builder()
            .organizationFiscalCode("77777777777")
            .iupd("mockIupd")
            .paymentOption(List.of(po))
            .build();
    expected.add(pp);
    when(mockedQuery.getResultList()).thenReturn(expected);
    when(mockedCountQuery.getSingleResult()).thenReturn(1L);

    doThrow(TableTransactionFailedException.class).when(scheduler).upsertPPTable(any(), any());

    try {
      // lancio il batch di archiviazione delle posizioni debitorie
      scheduler.manageDebtPositionsToHistoricize();
      fail();
    } catch (TableTransactionFailedException e) {
      verify(paymentPositionRepository, times(0)).deleteAll(any());
    }
  }

  @Test
  void manualUnhandledExceptionKOHistoricization() throws Exception {

    HistoricizationScheduler scheduler =
        spy(new HistoricizationScheduler(paymentPositionRepository));

    ReflectionTestUtils.setField(scheduler, "paginationMode", true);
    ReflectionTestUtils.setField(scheduler, "pageSize", 5);

    // precondition
    EntityManager entityManager = mock(EntityManager.class);
    doReturn(entityManager).when(scheduler).getEntityManager();

    TypedQuery<Long> mockedCountQuery = mock(TypedQuery.class);
    TypedQuery<PaymentPosition> mockedQuery = mock(TypedQuery.class);
    when(entityManager.createQuery(any(), eq(Long.class))).thenReturn(mockedCountQuery);
    when(entityManager.createQuery(any(), eq(PaymentPosition.class))).thenReturn(mockedQuery);
    when(mockedQuery.setParameter(anyInt(), any())).thenReturn(mockedQuery);
    when(mockedQuery.setFirstResult(anyInt())).thenReturn(mockedQuery);
    when(mockedQuery.setMaxResults(anyInt())).thenReturn(mockedQuery);
    when(mockedCountQuery.setParameter(anyInt(), any())).thenReturn(mockedCountQuery);

    List<PaymentPosition> expected = new ArrayList<>();
    PaymentOption po =
        PaymentOption.builder().iuv("mockIuv").paymentDate(LocalDateTime.now()).build();
    PaymentPosition pp =
        PaymentPosition.builder()
            .organizationFiscalCode("77777777777")
            .iupd("mockIupd")
            .paymentOption(List.of(po))
            .build();
    expected.add(pp);
    when(mockedQuery.getResultList()).thenReturn(expected);
    when(mockedCountQuery.getSingleResult()).thenReturn(1L);

    TableClient tc = mock(TableClient.class);
    doReturn(tc).when(scheduler).getTableClient(any(), any());
    doThrow(TableTransactionFailedException.class).when(tc).submitTransaction(any());

    try {
      // lancio il batch di archiviazione delle posizioni debitorie
      scheduler.manageDebtPositionsToHistoricize();
      fail();
    } catch (TableTransactionFailedException e) {
      verify(tc, times(1)).submitTransaction(any());
      verify(scheduler, times(1)).upsertPPTable(any(), any());
      verify(paymentPositionRepository, times(0)).deleteAll(any());
    }
  }
}
