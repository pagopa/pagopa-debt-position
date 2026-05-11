package it.gov.pagopa.debtposition.scheduler;

import static it.gov.pagopa.debtposition.util.SchedulerUtils.updateMDCError;
import static it.gov.pagopa.debtposition.util.SchedulerUtils.updateMDCForEndExecution;
import static it.gov.pagopa.debtposition.util.SchedulerUtils.updateMDCForStartExecution;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.function.BiFunction;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import it.gov.pagopa.debtposition.service.DebtPositionStatusBatchService;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;

@Component
@Slf4j
@Getter
@ConditionalOnProperty(name = "cron.job.schedule.enabled", havingValue = "true")
public class DebtPositionStatusScheduler {

  private final DebtPositionStatusBatchService batchService;

  private final int batchSize;

  private Thread threadOfExecution;

  public DebtPositionStatusScheduler(
      DebtPositionStatusBatchService batchService,
      @Value("${cron.job.schedule.batch.size:500}") int batchSize) {
    this.batchService = batchService;
    this.batchSize = batchSize;
  }

  @Scheduled(cron = "${cron.job.schedule.expression.valid.status}")
  @SchedulerLock(
      name = "changeDebtPositionStatusToValid",
      lockAtMostFor = "${cron.job.schedule.shedlock.lockatmostfor}",
      lockAtLeastFor = "${cron.job.schedule.shedlock.lockatleastfor}")
  public void changeDebtPositionStatusToValid() {
    updateMDCForStartExecution("changeDebtPositionStatusToValid", "");

    try {
      runBatchJob(
          "changeDebtPositionStatusToValid",
          batchService::updatePublishedToValidBatch);

      updateMDCForEndExecution();
    } catch (BatchJobException e) {
      updateMDCWithBatchContext(e);
      updateMDCError(e, "Valid Scheduler Error");
      throw e;
    } catch (Exception e) {
      updateMDCError(e, "Valid Scheduler Error");
      throw e;
    } finally {
      MDC.clear();
    }
  }

  @Scheduled(cron = "${cron.job.schedule.expression.expired.status}")
  @SchedulerLock(
      name = "changeDebtPositionStatusToExpired",
      lockAtMostFor = "${cron.job.schedule.shedlock.lockatmostfor}",
      lockAtLeastFor = "${cron.job.schedule.shedlock.lockatleastfor}")
  public void changeDebtPositionStatusToExpired() {
    updateMDCForStartExecution("changeDebtPositionStatusToExpired", "");

    try {
      runBatchJob(
          "changeDebtPositionStatusToExpired",
          batchService::updateValidToExpiredBatch);

      updateMDCForEndExecution();
    } catch (BatchJobException e) {
      updateMDCWithBatchContext(e);
      updateMDCError(e, "Expired Scheduler Error");
      throw e;
    } catch (Exception e) {
      updateMDCError(e, "Expired Scheduler Error");
      throw e;
    } finally {
      MDC.clear();
    }
  }

  private void runBatchJob(
      String operationName,
      BiFunction<LocalDateTime, Integer, Integer> batchOperation) {

    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);

    int batchIndex = 0;
    int totalAffectedRows = 0;
    int affectedRows;

    do {
      batchIndex++;

      try {
        affectedRows = batchOperation.apply(currentDate, batchSize);
      } catch (Exception e) {
        throw new BatchJobException(
            operationName,
            batchIndex,
            batchSize,
            totalAffectedRows,
            e);
      }

      totalAffectedRows += affectedRows;

      if (affectedRows > 0) {
        log.info(
            "{} - processed batchIndex={}, batchSize={}, affectedRows={}, totalAffectedRows={}",
            operationName,
            batchIndex,
            batchSize,
            affectedRows,
            totalAffectedRows);
      }

    } while (affectedRows == batchSize);

    this.threadOfExecution = Thread.currentThread();

    log.info(
        "{} completed. batchIndex={}, totalAffectedRows={}",
        operationName,
        batchIndex,
        totalAffectedRows);
  }

  private void updateMDCWithBatchContext(BatchJobException e) {
    MDC.put("batch.operationName", e.getOperationName());
    MDC.put("batch.index", String.valueOf(e.getBatchIndex()));
    MDC.put("batch.size", String.valueOf(e.getBatchSize()));
    MDC.put("batch.totalAffectedRows", String.valueOf(e.getTotalAffectedRows()));
  }

  private static class BatchJobException extends RuntimeException {

	private static final long serialVersionUID = -4793571952750239643L;
	private final String operationName;
    private final int batchIndex;
    private final int batchSize;
    private final int totalAffectedRows;

    private BatchJobException(
        String operationName,
        int batchIndex,
        int batchSize,
        int totalAffectedRows,
        Throwable cause) {
      super(
          String.format(
              "%s failed at batchIndex=%d, batchSize=%d, totalAffectedRows=%d",
              operationName,
              batchIndex,
              batchSize,
              totalAffectedRows),
          cause);
      this.operationName = operationName;
      this.batchIndex = batchIndex;
      this.batchSize = batchSize;
      this.totalAffectedRows = totalAffectedRows;
    }

    private String getOperationName() {
      return operationName;
    }

    private int getBatchIndex() {
      return batchIndex;
    }

    private int getBatchSize() {
      return batchSize;
    }

    private int getTotalAffectedRows() {
      return totalAffectedRows;
    }
  }
}