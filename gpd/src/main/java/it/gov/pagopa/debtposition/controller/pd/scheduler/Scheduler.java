package it.gov.pagopa.debtposition.controller.pd.scheduler;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class Scheduler {
	
	@Autowired
	private PaymentPositionRepository paymentPositionRepository;
	
	private static final String LOG_BASE_HEADER_INFO   = "[OperationType: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
	
	private Thread threadOfExecution;

	@Scheduled(cron = "${cron.job.schedule.expression.valid.status}")
	@Async
	@Transactional
	public void changeDebtPositionStatusToValid() {
		log.info(String.format(LOG_BASE_HEADER_INFO,"CRON JOB","changeDebtPositionStatusToValid", "Running at " + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(LocalDateTime.now())));
		LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
		paymentPositionRepository.updatePaymentPositionStatusToValid(currentDate, DebtPositionStatus.VALID);
		this.threadOfExecution = Thread.currentThread();
	}
	
	@Scheduled(cron = "${cron.job.schedule.expression.expired.status}")
	@Async
	@Transactional
	public void changeDebtPositionStatusToExpired() {
		log.info(String.format(LOG_BASE_HEADER_INFO,"CRON JOB","changeDebtPositionStatusToExpired", "Running at " + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(LocalDateTime.now())));
		LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
		paymentPositionRepository.updatePaymentPositionStatusToExpired(currentDate, DebtPositionStatus.EXPIRED);
		this.threadOfExecution = Thread.currentThread();
	}
	
	public Thread getThreadOfExecution() {
		return this.threadOfExecution;
	}
}
