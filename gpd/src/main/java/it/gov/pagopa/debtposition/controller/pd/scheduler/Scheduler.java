package it.gov.pagopa.debtposition.controller.pd.scheduler;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

@Component
@Slf4j
public class Scheduler {

    private static final String LOG_BASE_HEADER_INFO = "[OperationType: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
    private static final String CRON_JOB = "CRON JOB";
    @Autowired
    private PaymentPositionRepository paymentPositionRepository;
    private Thread threadOfExecution;

    @Scheduled(cron = "${cron.job.schedule.expression.valid.status}")
    @Async
    @Transactional
    public void changeDebtPositionStatusToValid() {
        log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, "changeDebtPositionStatusToValid", "Running at " + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(LocalDateTime.now())));
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        int numAffectedRows = paymentPositionRepository.updatePaymentPositionStatusToValid(currentDate, DebtPositionStatus.VALID);
        log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, "changeDebtPositionStatusToValid", "Number of updated rows " + numAffectedRows));
        this.threadOfExecution = Thread.currentThread();
    }

    @Scheduled(cron = "${cron.job.schedule.expression.expired.status}")
    @Async
    @Transactional
    public void changeDebtPositionStatusToExpired() {
        log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, "changeDebtPositionStatusToExpired", "Running at " + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(LocalDateTime.now())));
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        int numAffectedRows = paymentPositionRepository.updatePaymentPositionStatusToExpired(currentDate, DebtPositionStatus.EXPIRED);
        log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, "changeDebtPositionStatusToExpired", "Number of updated rows " + numAffectedRows));
        this.threadOfExecution = Thread.currentThread();
    }

    public Thread getThreadOfExecution() {
        return this.threadOfExecution;
    }
}
