package it.gov.pagopa.debtposition.scheduler;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import static it.gov.pagopa.debtposition.util.SchedulerUtils.updateMDCError;
import static it.gov.pagopa.debtposition.util.SchedulerUtils.updateMDCForEndExecution;
import static it.gov.pagopa.debtposition.util.SchedulerUtils.updateMDCForStartExecution;
import jakarta.transaction.Transactional;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
@Getter
@ConditionalOnProperty(name = "cron.job.schedule.enabled", matchIfMissing = true)
public class ExpiredPositionsScheduler {

  private static final String LOG_BASE_HEADER_INFO =
      "[OperationType: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
  private static final String CRON_JOB = "CRON JOB";
  @Autowired private PaymentPositionRepository paymentPositionRepository;
  private Thread threadOfExecution;

  // Helper method for batch processing to prevent connection pool exhaustion
  @Transactional
  private int updatePaymentPositionStatusToExpiredBatch(
      LocalDateTime currentDate, int batchSize) {
    return paymentPositionRepository.updatePaymentPositionStatusToExpiredWithLimit(
        currentDate, batchSize);
  }

  @Scheduled(cron = "${cron.job.schedule.expression.valid.status}")
  @Async
  @Transactional
  public void changeDebtPositionStatusToValid() {
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            CRON_JOB,
            "changeDebtPositionStatusToValid",
            "Running at "
                + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(LocalDateTime.now())));
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    int numAffectedRows =
        paymentPositionRepository.updatePaymentPositionStatusToValid(
            currentDate, DebtPositionStatus.VALID);
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            CRON_JOB,
            "changeDebtPositionStatusToValid",
            "Number of updated rows " + numAffectedRows));
    this.threadOfExecution = Thread.currentThread();
  }

  @Scheduled(cron = "${cron.job.schedule.expression.expired.status}")
  @Async
  public void changeDebtPositionStatusToExpired() {
    updateMDCForStartExecution("changeDebtPositionStatusToExpired", "");
    try {
      log.debug(
          String.format(
              LOG_BASE_HEADER_INFO,
              CRON_JOB,
              "changeDebtPositionStatusToExpired",
              "Running at "
                  + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
                      .format(LocalDateTime.now())));
      LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
      // OPTIMIZATION: Update in batches to prevent connection pool exhaustion
      int totalAffectedRows = 0;
      int batchSize = 1000;
      int batchAffectedRows = batchSize;
      
      while (batchAffectedRows >= batchSize) {
        batchAffectedRows = updatePaymentPositionStatusToExpiredBatch(currentDate, batchSize);
        totalAffectedRows += batchAffectedRows;
        if (batchAffectedRows > 0) {
          log.debug(
              String.format(
                  LOG_BASE_HEADER_INFO,
                  CRON_JOB,
                  "changeDebtPositionStatusToExpired",
                  "Batch processed: " + batchAffectedRows + " rows, total: " + totalAffectedRows));
        }
      }
      log.debug(
          String.format(
              LOG_BASE_HEADER_INFO,
              CRON_JOB,
              "changeDebtPositionStatusToExpired",
              "Number of updated rows " + totalAffectedRows));
      this.threadOfExecution = Thread.currentThread();
      updateMDCForEndExecution();
    } catch (Exception e) {
      updateMDCError(e, "Expired Scheduler Scheduler Error");
      throw e;
    } finally {
      MDC.clear();
    }
  }
}
