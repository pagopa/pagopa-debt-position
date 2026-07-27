package it.gov.pagopa.debtposition.scheduler;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import it.gov.pagopa.debtposition.service.DebtPositionStatusBatchService;
import it.gov.pagopa.debtposition.util.SchedulerUtils;
import java.time.LocalDateTime;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.junit.jupiter.api.extension.ExtendWith;
import org.slf4j.MDC;

@ExtendWith(MockitoExtension.class)
class DebtPositionStatusSchedulerUnitTest {

  @Mock
  private DebtPositionStatusBatchService batchService;

  private DebtPositionStatusScheduler scheduler;

  @BeforeEach
  void setUp() {
    scheduler = new DebtPositionStatusScheduler(batchService, 2);
    MDC.clear();
  }

  @Test
  void changeDebtPositionStatusToValid_shouldProcessBatchesUntilLastBatchIsSmallerThanBatchSize() {
    when(batchService.updatePublishedToValidBatch(any(LocalDateTime.class), eq(2)))
        .thenReturn(2, 2, 1);

    scheduler.changeDebtPositionStatusToValid();

    verify(batchService, times(3))
        .updatePublishedToValidBatch(any(LocalDateTime.class), eq(2));

    assertThat(scheduler.getBatchSize()).isEqualTo(2);
    assertThat(scheduler.getThreadOfExecution()).isEqualTo(Thread.currentThread());
    assertThat(MDC.getCopyOfContextMap()).isNullOrEmpty();
  }

  @Test
  void changeDebtPositionStatusToExpired_shouldProcessBatchesUntilZeroRowsAreAffected() {
    when(batchService.updateValidToExpiredBatch(any(LocalDateTime.class), eq(2)))
        .thenReturn(2, 0);

    scheduler.changeDebtPositionStatusToExpired();

    verify(batchService, times(2))
        .updateValidToExpiredBatch(any(LocalDateTime.class), eq(2));

    assertThat(scheduler.getThreadOfExecution()).isEqualTo(Thread.currentThread());
    assertThat(MDC.getCopyOfContextMap()).isNullOrEmpty();
  }

  @Test
  void changeDebtPositionStatusToValid_whenBatchOperationFails_shouldWrapExceptionWithBatchContext() {
    RuntimeException dbException = new RuntimeException("DB error");

    when(batchService.updatePublishedToValidBatch(any(LocalDateTime.class), eq(2)))
        .thenReturn(2)
        .thenThrow(dbException);

    assertThatThrownBy(() -> scheduler.changeDebtPositionStatusToValid())
        .isInstanceOf(RuntimeException.class)
        .hasMessageContaining("changeDebtPositionStatusToValid failed at batchIndex=2")
        .hasMessageContaining("batchSize=2")
        .hasMessageContaining("totalAffectedRows=2")
        .hasCause(dbException);

    verify(batchService, times(2))
        .updatePublishedToValidBatch(any(LocalDateTime.class), eq(2));

    assertThat(MDC.getCopyOfContextMap()).isNullOrEmpty();
  }

  @Test
  void changeDebtPositionStatusToExpired_whenBatchOperationFails_shouldWrapExceptionWithBatchContext() {
    RuntimeException dbException = new RuntimeException("DB error");

    when(batchService.updateValidToExpiredBatch(any(LocalDateTime.class), eq(2)))
        .thenReturn(2)
        .thenThrow(dbException);

    assertThatThrownBy(() -> scheduler.changeDebtPositionStatusToExpired())
        .isInstanceOf(RuntimeException.class)
        .hasMessageContaining("changeDebtPositionStatusToExpired failed at batchIndex=2")
        .hasMessageContaining("batchSize=2")
        .hasMessageContaining("totalAffectedRows=2")
        .hasCause(dbException);

    verify(batchService, times(2))
        .updateValidToExpiredBatch(any(LocalDateTime.class), eq(2));

    assertThat(MDC.getCopyOfContextMap()).isNullOrEmpty();
  }

  @Test
  void changeDebtPositionStatusToValid_whenUnexpectedExceptionOccurs_shouldRethrowAndClearMDC() {
    when(batchService.updatePublishedToValidBatch(any(LocalDateTime.class), eq(2)))
        .thenReturn(0);

    try (MockedStatic<SchedulerUtils> schedulerUtils =
        mockStatic(SchedulerUtils.class, CALLS_REAL_METHODS)) {

      IllegalStateException unexpectedException =
          new IllegalStateException("Unexpected MDC end error");

      schedulerUtils
          .when(SchedulerUtils::updateMDCForEndExecution)
          .thenThrow(unexpectedException);

      assertThatThrownBy(() -> scheduler.changeDebtPositionStatusToValid())
          .isSameAs(unexpectedException);

      verify(batchService, times(1))
          .updatePublishedToValidBatch(any(LocalDateTime.class), eq(2));
    }

    assertThat(MDC.getCopyOfContextMap()).isNullOrEmpty();
  }

  @Test
  void changeDebtPositionStatusToExpired_whenUnexpectedExceptionOccurs_shouldRethrowAndClearMDC() {
    when(batchService.updateValidToExpiredBatch(any(LocalDateTime.class), eq(2)))
        .thenReturn(0);

    try (MockedStatic<SchedulerUtils> schedulerUtils =
        mockStatic(SchedulerUtils.class, CALLS_REAL_METHODS)) {

      IllegalStateException unexpectedException =
          new IllegalStateException("Unexpected MDC end error");

      schedulerUtils
          .when(SchedulerUtils::updateMDCForEndExecution)
          .thenThrow(unexpectedException);

      assertThatThrownBy(() -> scheduler.changeDebtPositionStatusToExpired())
          .isSameAs(unexpectedException);

      verify(batchService, times(1))
          .updateValidToExpiredBatch(any(LocalDateTime.class), eq(2));
    }

    assertThat(MDC.getCopyOfContextMap()).isNullOrEmpty();
  }
}