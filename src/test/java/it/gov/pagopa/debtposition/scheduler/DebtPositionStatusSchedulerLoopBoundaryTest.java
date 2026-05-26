package it.gov.pagopa.debtposition.scheduler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import it.gov.pagopa.debtposition.service.DebtPositionStatusBatchService;
import java.time.LocalDateTime;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class DebtPositionStatusSchedulerLoopBoundaryTest {

  private static final int BATCH_SIZE = 2;

  @Mock
  private DebtPositionStatusBatchService batchService;

  private DebtPositionStatusScheduler scheduler;

  @BeforeEach
  void setUp() {
    // The scheduler is instantiated directly to verify the loop boundary without loading the Spring context.
    scheduler = new DebtPositionStatusScheduler(batchService, BATCH_SIZE);
  }

  @Test
  void changeDebtPositionStatusToValidShouldContinueWhileAffectedRowsEqualsBatchSize() {
    // The batch service returns exactly batchSize twice, then fewer rows.
    // Expected behavior: the scheduler keeps looping while affectedRows == batchSize
    // and stops only when affectedRows < batchSize.
    when(batchService.updatePublishedToValidBatch(any(LocalDateTime.class), eq(BATCH_SIZE)))
        .thenReturn(BATCH_SIZE, BATCH_SIZE, 1);

    scheduler.changeDebtPositionStatusToValid();

    verify(batchService, times(3))
        .updatePublishedToValidBatch(any(LocalDateTime.class), eq(BATCH_SIZE));
  }

  @Test
  void changeDebtPositionStatusToValidShouldStopImmediatelyWhenAffectedRowsIsLowerThanBatchSize() {
    // The first execution returns fewer rows than batchSize.
    // Expected behavior: the scheduler stops after the first iteration.
    when(batchService.updatePublishedToValidBatch(any(LocalDateTime.class), eq(BATCH_SIZE)))
        .thenReturn(1);

    scheduler.changeDebtPositionStatusToValid();

    verify(batchService, times(1))
        .updatePublishedToValidBatch(any(LocalDateTime.class), eq(BATCH_SIZE));
  }

  @Test
  void changeDebtPositionStatusToExpiredShouldUseSameLoopBoundaryLogic() {
    // Same loop boundary test for the VALID -> EXPIRED batch.
    // The first execution reaches the batch limit, the second one returns zero rows,
    // so the scheduler must execute exactly two iterations.
    when(batchService.updateValidToExpiredBatch(any(LocalDateTime.class), eq(BATCH_SIZE)))
        .thenReturn(BATCH_SIZE, 0);

    scheduler.changeDebtPositionStatusToExpired();

    verify(batchService, times(2))
        .updateValidToExpiredBatch(any(LocalDateTime.class), eq(BATCH_SIZE));
  }
}