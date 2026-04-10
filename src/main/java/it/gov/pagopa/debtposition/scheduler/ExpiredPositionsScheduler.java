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
@ConditionalOnProperty(name = "cron.job.schedule.enabled", matchIfMissing = true)
public class ExpiredPositionsScheduler {
   
   private final DebtPositionStatusBatchService batchService;

   private final int batchSize;

   private Thread threadOfExecution;

   public ExpiredPositionsScheduler(
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

	int totalAffectedRows = 0;
	int affectedRows;

    do {
      affectedRows = batchOperation.apply(currentDate, batchSize);
      totalAffectedRows += affectedRows;

      if (affectedRows > 0) {
        log.info(
            "{} - processed batchSize={}, affectedRows={}, totalAffectedRows={}",
            operationName,
            batchSize,
            affectedRows,
            totalAffectedRows);
      }

    } while (affectedRows == batchSize);

    this.threadOfExecution = Thread.currentThread();

    log.info(
        "{} completed. totalAffectedRows={}",
        operationName,
        totalAffectedRows);
  }
}