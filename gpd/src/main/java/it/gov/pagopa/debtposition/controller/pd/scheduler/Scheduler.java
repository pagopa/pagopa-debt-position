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

	@Scheduled(cron = "${cron.expression}")
	@Async
	@Transactional
	public void changeDebtPositionStatus() {
		log.info(String.format(LOG_BASE_HEADER_INFO,"CRON JOB","changeDebtPositionStatus", DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(LocalDateTime.now())));
		LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
		paymentPositionRepository.updatePaymentPositionStatus(currentDate, DebtPositionStatus.VALID);
	}
}
