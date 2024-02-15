package it.gov.pagopa.debtposition.scheduler;


import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.util.ReflectionTestUtils;

import com.microsoft.azure.storage.StorageErrorCode;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.TableBatchOperation;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.config.SchedulerConfig;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;

@SpringBootTest(classes = DebtPositionApplication.class)
@SpringJUnitConfig(SchedulerConfig.class)
class HistoricizationSchedulerTest {

	@MockBean
	PaymentPositionRepository paymentPositionRepository;

	// azure storage params
	@Value("${azure.archive.storage.connection}")
	private String archiveStorageConnection;



	@Test
	void manualTotalEntriesHistoricization() throws Exception  {

		HistoricizationScheduler scheduler = spy(new HistoricizationScheduler(paymentPositionRepository));

		ReflectionTestUtils.setField(scheduler, "paginationMode", false);

		//precondition
		EntityManager entityManager = mock(EntityManager.class);
		doReturn(entityManager).when(scheduler).getEntityManager();

		TypedQuery<PaymentPosition> mockedQuery	= (TypedQuery<PaymentPosition>) mock(TypedQuery.class);
		when(entityManager.createQuery(any(), eq(PaymentPosition.class))).thenReturn(mockedQuery);
		when(mockedQuery.setParameter(anyInt(), any())).thenReturn(mockedQuery);

		List<PaymentPosition> expected = new ArrayList<>();
		PaymentOption po = PaymentOption.builder().iuv("mockIuv").paymentDate(LocalDateTime.now()).build();
		PaymentPosition pp = PaymentPosition.builder().organizationFiscalCode("77777777777").iupd("mockIupd").paymentOption(List.of(po)).build();
		expected.add(pp);
		when(mockedQuery.getResultList()).thenReturn(expected);

		doNothing().when(scheduler).upsertPOTable(any());
		doNothing().when(scheduler).upsertPPTable(any(), any());

		// lancio il batch di archiviazione delle posizioni debitorie
		scheduler.manageDebtPositionsToHistoricize();

		verify(scheduler, times(1)).upsertPOTable(any());
		verify(scheduler, times(1)).upsertPPTable(any(), any());
		verify(paymentPositionRepository, times(1)).deleteAll(any());
	}

	@Test
	void manualPaginatedEntriesHistoricization() throws Exception  {

		HistoricizationScheduler scheduler = spy(new HistoricizationScheduler(paymentPositionRepository));

		ReflectionTestUtils.setField(scheduler, "paginationMode", true);
		ReflectionTestUtils.setField(scheduler, "pageSize", 5);

		//precondition
		EntityManager entityManager = mock(EntityManager.class);
		doReturn(entityManager).when(scheduler).getEntityManager();

		TypedQuery<Long>            mockedCountQuery = (TypedQuery<Long>) mock(TypedQuery.class);
		TypedQuery<PaymentPosition> mockedQuery	     = (TypedQuery<PaymentPosition>) mock(TypedQuery.class);
		when(entityManager.createQuery(any(), eq(Long.class))).thenReturn(mockedCountQuery);
		when(entityManager.createQuery(any(), eq(PaymentPosition.class))).thenReturn(mockedQuery);
		when(mockedQuery.setParameter(anyInt(), any())).thenReturn(mockedQuery);
		when(mockedQuery.setFirstResult(anyInt())).thenReturn(mockedQuery);
		when(mockedQuery.setMaxResults(anyInt())).thenReturn(mockedQuery);
		when(mockedCountQuery.setParameter(anyInt(), any())).thenReturn(mockedCountQuery);

		List<PaymentPosition> expected = new ArrayList<>();
		PaymentOption po = PaymentOption.builder().iuv("mockIuv").paymentDate(LocalDateTime.now()).build();
		PaymentPosition pp = PaymentPosition.builder().organizationFiscalCode("77777777777").iupd("mockIupd").paymentOption(List.of(po)).build();
		expected.add(pp);
		when(mockedQuery.getResultList()).thenReturn(expected);
		when(mockedCountQuery.getSingleResult()).thenReturn(1L);

		doNothing().when(scheduler).upsertPOTable(any());
		doNothing().when(scheduler).upsertPPTable(any(), any());

		// lancio il batch di archiviazione delle posizioni debitorie
		scheduler.manageDebtPositionsToHistoricize();

		verify(scheduler, times(1)).upsertPOTable(any());
		verify(scheduler, times(1)).upsertPPTable(any(), any());
		verify(paymentPositionRepository, times(1)).deleteAll(any());
	}

	@Test
	void manualKOHistoricization() throws Exception  {

		HistoricizationScheduler scheduler = spy(new HistoricizationScheduler(paymentPositionRepository));

		ReflectionTestUtils.setField(scheduler, "paginationMode", true);
		ReflectionTestUtils.setField(scheduler, "pageSize", 5);

		//precondition
		EntityManager entityManager = mock(EntityManager.class);
		doReturn(entityManager).when(scheduler).getEntityManager();

		TypedQuery<Long>            mockedCountQuery = (TypedQuery<Long>) mock(TypedQuery.class);
		TypedQuery<PaymentPosition> mockedQuery	     = (TypedQuery<PaymentPosition>) mock(TypedQuery.class);
		when(entityManager.createQuery(any(), eq(Long.class))).thenReturn(mockedCountQuery);
		when(entityManager.createQuery(any(), eq(PaymentPosition.class))).thenReturn(mockedQuery);
		when(mockedQuery.setParameter(anyInt(), any())).thenReturn(mockedQuery);
		when(mockedQuery.setFirstResult(anyInt())).thenReturn(mockedQuery);
		when(mockedQuery.setMaxResults(anyInt())).thenReturn(mockedQuery);
		when(mockedCountQuery.setParameter(anyInt(), any())).thenReturn(mockedCountQuery);

		List<PaymentPosition> expected = new ArrayList<>();
		PaymentOption po = PaymentOption.builder().iuv("mockIuv").paymentDate(LocalDateTime.now()).build();
		PaymentPosition pp = PaymentPosition.builder().organizationFiscalCode("77777777777").iupd("mockIupd").paymentOption(List.of(po)).build();
		expected.add(pp);
		when(mockedQuery.getResultList()).thenReturn(expected);
		when(mockedCountQuery.getSingleResult()).thenReturn(1L);

		doThrow(StorageException.class).when(scheduler).upsertPOTable(any());

		try {
			// lancio il batch di archiviazione delle posizioni debitorie
			scheduler.manageDebtPositionsToHistoricize();
			fail();
		} catch (StorageException e) {
			verify(scheduler, times(1)).upsertPOTable(any());
			verify(scheduler, times(0)).upsertPPTable(any(), any());
			verify(paymentPositionRepository, times(0)).deleteAll(any());
		}
	}

	
	@Test
	void manualUnhandledExceptionKOHistoricization() throws Exception  {

		HistoricizationScheduler scheduler = spy(new HistoricizationScheduler(paymentPositionRepository));

		ReflectionTestUtils.setField(scheduler, "paginationMode", true);
		ReflectionTestUtils.setField(scheduler, "pageSize", 5);

		//precondition
		EntityManager entityManager = mock(EntityManager.class);
		doReturn(entityManager).when(scheduler).getEntityManager();

		TypedQuery<Long>            mockedCountQuery = (TypedQuery<Long>) mock(TypedQuery.class);
		TypedQuery<PaymentPosition> mockedQuery	     = (TypedQuery<PaymentPosition>) mock(TypedQuery.class);
		when(entityManager.createQuery(any(), eq(Long.class))).thenReturn(mockedCountQuery);
		when(entityManager.createQuery(any(), eq(PaymentPosition.class))).thenReturn(mockedQuery);
		when(mockedQuery.setParameter(anyInt(), any())).thenReturn(mockedQuery);
		when(mockedQuery.setFirstResult(anyInt())).thenReturn(mockedQuery);
		when(mockedQuery.setMaxResults(anyInt())).thenReturn(mockedQuery);
		when(mockedCountQuery.setParameter(anyInt(), any())).thenReturn(mockedCountQuery);

		List<PaymentPosition> expected = new ArrayList<>();
		PaymentOption po = PaymentOption.builder().iuv("mockIuv").paymentDate(LocalDateTime.now()).build();
		PaymentPosition pp = PaymentPosition.builder().organizationFiscalCode("77777777777").iupd("mockIupd").paymentOption(List.of(po)).build();
		expected.add(pp);
		when(mockedQuery.getResultList()).thenReturn(expected);
		when(mockedCountQuery.getSingleResult()).thenReturn(1L);

		StorageException sExc = new StorageException(StorageErrorCode.SERVICE_TIMEOUT.toString(), "mock error", null);
		CloudTable ct = mock(CloudTable.class);
		doReturn(ct).when(scheduler).getCloudTableClient(any(), any());
		doThrow(sExc).when(ct).execute(any(TableBatchOperation.class));

		try {
			// lancio il batch di archiviazione delle posizioni debitorie
			scheduler.manageDebtPositionsToHistoricize();
			fail();
		} catch (StorageException e) {
			verify(scheduler, times(1)).upsertPOTable(any());
			verify(ct, times(1)).execute(any(TableBatchOperation.class));
			verify(scheduler, times(0)).upsertPPTable(any(), any());
			verify(paymentPositionRepository, times(0)).deleteAll(any());
		}  
	}
}
