package it.gov.pagopa.debtposition.scheduler;


import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.TimeUnit;

import org.awaitility.Awaitility;
import org.hamcrest.core.IsNull;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.config.SchedulerConfig;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.mock.DebtPositionMock;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
@SpringBootTest(classes = DebtPositionApplication.class)
@SpringJUnitConfig(SchedulerConfig.class)
@AutoConfigureMockMvc
class ExpiredPositionsSchedulerTest {

	@Autowired
	ExpiredPositionsScheduler expiredPositionsScheduler;

	@Autowired
	private MockMvc mvc;

	@Test
	void manualChangeDebtPositionStatusToValid() 
			throws Exception {

		// creo una posizione debitoria (con 'validity date')
		mvc.perform(post("/organizations/SCHEDULEVALID_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock5())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata lo stato della posizione debitoria
		mvc.perform(post("/organizations/SCHEDULEVALID_12345678901/debtpositions/12345678901IUPDMOCK3/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato effettivamente aggiornato a PUBLISHED 
		mvc.perform(get("/organizations/SCHEDULEVALID_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PUBLISHED.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

		// introduco un ritardo in modo da far scadere la validity date
		LocalDateTime currentDatePlusSeconds = LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS);
		Awaitility.await().until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(currentDatePlusSeconds));

		// lancio il batch per consentire il passaggio di stato
		expiredPositionsScheduler.changeDebtPositionStatusToValid();

		// attendo che il thread asincrono sia attivo
		Awaitility.await()
		.atMost(3, TimeUnit.SECONDS)
		.pollInterval(15, TimeUnit.MILLISECONDS)
		.until(() -> expiredPositionsScheduler.getThreadOfExecution() != null);

		// dopo che il batch ha terminato verifico che lo stato sia effettivamente passato a VALID
		mvc.perform(get("/organizations/SCHEDULEVALID_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()));
	}

	@Test
	void manualChangeDebtPositionStatusToValidAfterDueDate() 
			throws Exception {

		// creo una posizione debitoria (con 'validity date') senza valorizzare il campo switchToExpired (quindi per default verrà messo a false) -> Lo stato deve rimanere VALID passata la due_date
		mvc.perform(post("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock5())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata lo stato della posizione debitoria
		mvc.perform(post("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions/12345678901IUPDMOCK3/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato effettivamente aggiornato a PUBLISHED 
		mvc.perform(get("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PUBLISHED.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

		// introduco un ritardo in modo da far scadere la validity date
		LocalDateTime currentDatePlusSeconds = LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS);
		Awaitility.await().until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(currentDatePlusSeconds));

		// lancio il batch per consentire il passaggio di stato
		expiredPositionsScheduler.changeDebtPositionStatusToValid();

		Awaitility.await()
		.atMost(3, TimeUnit.SECONDS)
		.pollInterval(15, TimeUnit.MILLISECONDS)
		.until(() -> expiredPositionsScheduler.getThreadOfExecution() != null);

		// dopo che il batch ha terminato verifico che lo stato sia effettivamente passato a VALID
		mvc.perform(get("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()));

		// introduco un nuovo ritardo in modo da far scadere la due date
		LocalDateTime newCurrentDatePlusSeconds = LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS);
		Awaitility.await().until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(newCurrentDatePlusSeconds));

		// lancio il batch per consentire il passaggio di stato
		expiredPositionsScheduler.changeDebtPositionStatusToExpired();

		// attendo che il thread asincrono sia attivo
		Awaitility.await()
		.atMost(3, TimeUnit.SECONDS)
		.pollInterval(15, TimeUnit.MILLISECONDS)
		.until(() -> expiredPositionsScheduler.getThreadOfExecution() != null);

		// verifico che lo stato sia rimasto a VALID anche se now > due_date 
		mvc.perform(get("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()));
	}
	
	@Test
	void manualChangeDebtPositionStatusToExpiredAfterDueDate() 
			throws Exception {

		// creo una posizione debitoria (con 'validity date') valorizzando il campo switchToExpired a true -> Lo stato deve passare ad EXPIRED passata la due_date
		mvc.perform(post("/organizations/SCHEDULEEXP_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock7())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata lo stato della posizione debitoria
		mvc.perform(post("/organizations/SCHEDULEEXP_12345678901/debtpositions/12345678901IUPDMOCK3/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato effettivamente aggiornato a PUBLISHED 
		mvc.perform(get("/organizations/SCHEDULEEXP_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PUBLISHED.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

		// introduco un ritardo in modo da far scadere la validity date
		LocalDateTime currentDatePlusSeconds = LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS);
		Awaitility.await().until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(currentDatePlusSeconds));

		// lancio il batch per consentire il passaggio di stato
		expiredPositionsScheduler.changeDebtPositionStatusToValid();

		Awaitility.await()
		.atMost(3, TimeUnit.SECONDS)
		.pollInterval(15, TimeUnit.MILLISECONDS)
		.until(() -> expiredPositionsScheduler.getThreadOfExecution() != null);

		// dopo che il batch ha terminato verifico che lo stato sia effettivamente passato a VALID
		mvc.perform(get("/organizations/SCHEDULEEXP_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()));

		// introduco un nuovo ritardo in modo da far scadere la due date
		LocalDateTime newCurrentDatePlusSeconds = LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS);
		Awaitility.await().until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(newCurrentDatePlusSeconds));

		// lancio il batch per consentire il passaggio di stato
		expiredPositionsScheduler.changeDebtPositionStatusToExpired();

		// attendo che il thread asincrono sia attivo
		Awaitility.await()
		.atMost(3, TimeUnit.SECONDS)
		.pollInterval(15, TimeUnit.MILLISECONDS)
		.until(() -> expiredPositionsScheduler.getThreadOfExecution() != null);

		// verifico che lo stato sia passato ad EXPIRED
		mvc.perform(get("/organizations/SCHEDULEEXP_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.EXPIRED.toString()));
		
	}
	
	@Test
	void manualChangeDebtPositionStatusToExpiredAndUpdateAllowed() 
			throws Exception {

		PaymentPositionDTO pp7 = DebtPositionMock.getMock7();
		
		// creo una posizione debitoria (con 'validity date') valorizzando il campo switchToExpired a true -> Lo stato deve passare ad EXPIRED passata la due_date
		mvc.perform(post("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions")
				.content(TestUtil.toJson(pp7)).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata lo stato della posizione debitoria
		mvc.perform(post("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato effettivamente aggiornato a PUBLISHED 
		mvc.perform(get("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PUBLISHED.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

		// introduco un ritardo in modo da far scadere la validity date
		LocalDateTime currentDatePlusSeconds = LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS);
		Awaitility.await().until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(currentDatePlusSeconds));

		// lancio il batch per consentire il passaggio di stato
		expiredPositionsScheduler.changeDebtPositionStatusToValid();

		Awaitility.await()
		.atMost(3, TimeUnit.SECONDS)
		.pollInterval(15, TimeUnit.MILLISECONDS)
		.until(() -> expiredPositionsScheduler.getThreadOfExecution() != null);

		// dopo che il batch ha terminato verifico che lo stato sia effettivamente passato a VALID
		mvc.perform(get("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()));

		// introduco un nuovo ritardo in modo da far scadere la due date
		LocalDateTime newCurrentDatePlusSeconds = LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS);
		Awaitility.await().until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(newCurrentDatePlusSeconds));

		// lancio il batch per consentire il passaggio di stato
		expiredPositionsScheduler.changeDebtPositionStatusToExpired();

		// attendo che il thread asincrono sia attivo
		Awaitility.await()
		.atMost(3, TimeUnit.SECONDS)
		.pollInterval(15, TimeUnit.MILLISECONDS)
		.until(() -> expiredPositionsScheduler.getThreadOfExecution() != null);

		// verifico che lo stato sia passato ad EXPIRED
		mvc.perform(get("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.EXPIRED.toString()));

		// aggiorno la posizione debitoria (stato atteso DRAFT)
		pp7.setCompanyName("Comune di Napoli");
		pp7.getPaymentOption().get(0).setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.SECONDS));
		mvc.perform(put("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
				.content(TestUtil.toJson(pp7))
				.contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isOk())
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.DRAFT.toString()));
		
		// aggiorno la posizione debitoria e ne richiedo la pubblicazione (stato atteso PUBLISHED)
		pp7.setCompanyName("Comune di Milano");
		pp7.getPaymentOption().get(0).setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.SECONDS));
		mvc.perform(put("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3?toPublish=True")
				.content(TestUtil.toJson(pp7))
				.contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isOk())
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PUBLISHED.toString()));
		
		// aggiorno la posizione debitoria con un body che non contiene la 'validity date' e ne richiedo la pubblicazione (stato atteso VALID)
		pp7.setCompanyName("Comune di Latina");
		pp7.setValidityDate(null);
		mvc.perform(put("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3?toPublish=True")
				.content(TestUtil.toJson(pp7))
				.contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isOk())
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()));
		
		// recupero la posizione debitoria e verifico che lo stato sia in VALID e il dato sia aggiornato all'ultimo valore
		mvc.perform(get("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.notNullValue()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.companyName").value("Comune di Latina"));
	}

}
