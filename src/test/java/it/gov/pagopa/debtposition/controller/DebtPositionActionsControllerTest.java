package it.gov.pagopa.debtposition.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;

import org.awaitility.Awaitility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.mock.DebtPositionMock;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class DebtPositionActionsControllerTest {

	@Autowired
	private MockMvc mvc;

	@Mock
	private ModelMapper modelMapperMock;

	@BeforeEach
	void setUp() {
	}



	/**
	 * PUBLISH DEBT POSITION
	 */
	@Test
	void publishDebtPosition_to_valid_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/PBHVALID_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico lo stato in draft
		mvc.perform(get("/organizations/PBHVALID_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.DRAFT.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PBHVALID_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato aggiornato a valid (doppio passaggio di stato) 
		mvc.perform(get("/organizations/PBHVALID_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());
	}

	@Test
	void publishDebtPosition_to_publish_200() throws Exception {
		// creo una posizione debitoria (con 'validity date')
		mvc.perform(post("/organizations/PBHPUBLISH_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock5())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico lo stato in draft
		mvc.perform(get("/organizations/PBHPUBLISH_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.DRAFT.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

		// porto in pubblicata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PBHPUBLISH_12345678901/debtpositions/12345678901IUPDMOCK3/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato aggiornato a publish (singolo passaggio di stato) 
		mvc.perform(get("/organizations/PBHPUBLISH_12345678901/debtpositions/12345678901IUPDMOCK3")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PUBLISHED.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());
	}

	@Test
	void publishDebtPosition_409() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/PBH409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico lo stato in draft
		mvc.perform(get("/organizations/PBH409_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.DRAFT.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PBH409_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato aggiornato a valid (doppio passaggio di stato) 
		mvc.perform(get("/organizations/PBH409_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

		// provo a fare una nuova pubblicazione su una posizione debitoria con uno stato non più idoneo
		mvc.perform(post("/organizations/PBH409_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
	}

	@Test
	void publishDebtPosition_404() throws Exception {	
		// chiamata per portare in pubblicata una posizione debitoria con IUPD inesistente
		mvc.perform(post("/organizations/PBH_12345678901/debtpositions/12345678901IUPD404MOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound());
	}
	
	@Test
	void publishDebtPosition_min_due_date_409() throws Exception {
		
		// creo una posizione debitoria (senza 'validity date' impostata) e con due_date di pochissimo più grande della current_date
		mvc.perform(post("/organizations/PBH409DUEDATE_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.get409_Min_Due_Date_Mock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico lo stato
		mvc.perform(get("/organizations/PBH409DUEDATE_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.DRAFT.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty())
		.andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").isEmpty());
		
		// introduco un ritardo in modo da far scadere la min_due_date
		LocalDateTime currentDatePlusSeconds = LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS);
		Awaitility.await().until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(currentDatePlusSeconds));

		// porto in pubblicata lo stato della posizione debitoria => devo ottenere 409 per il fatto che currentDate > due_date
		mvc.perform(post("/organizations/PBH409DUEDATE_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());

	}

	@Test
	void publishDebtPosition_min_valid_date_409() throws Exception {

		// creo una posizione debitoria (con 'validity date' impostata) e con due_date di pochissimo più grande della current_date
		mvc.perform(post("/organizations/PIVA12345678/debtpositions")
						.content(TestUtil.toJson(DebtPositionMock.get409_Valid_Date_Mock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico lo stato
		mvc.perform(get("/organizations/PIVA12345678/debtpositions/12345678901IUPDMOCK4")
						.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.status")
						.value(DebtPositionStatus.DRAFT.toString()))
				.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

		// introduco un ritardo in modo da far scadere la validity_date
		LocalDateTime currentDatePlusSeconds = LocalDateTime.now(ZoneOffset.UTC).plus(5, ChronoUnit.SECONDS);
		Awaitility.await().until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(currentDatePlusSeconds));

		// porto in pubblicata lo stato della posizione debitoria => devo ottenere 409 per il fatto che currentDate > due_date
		mvc.perform(post("/organizations/PIVA12345678/debtpositions/12345678901IUPDMOCK4/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());

	}


	/**
	 * INVALIDATE DEBT POSITION
	 */
	@Test
	void invalidateDebtPosition_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/INVALIDATE_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico lo stato in draft
		mvc.perform(get("/organizations/INVALIDATE_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.DRAFT.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/INVALIDATE_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato aggiornato a valid (doppio passaggio di stato) 
		mvc.perform(get("/organizations/INVALIDATE_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

		// invalido la posizione debitoria
		mvc.perform(post("/organizations/INVALIDATE_12345678901/debtpositions/12345678901IUPDMOCK1/invalidate")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.INVALID.toString()));
	}
	
	@Test
	void invalidateDebtPosition_404() throws Exception {	
		// chiamata per portare in invalidata una posizione debitoria con IUPD inesistente
		mvc.perform(post("/organizations/INVALIDATE404_12345678901/debtpositions/12345678901IUPD404MOCK1/invalidate")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound());
	}

	@Test
	void invalidateDebtPosition_409() throws Exception {
		// creo una posizione debitoria 
		mvc.perform(post("/organizations/INVALIDATE409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// invalido la posizione debitoria ancora in stato DRAFT -> errore 409
		mvc.perform(post("/organizations/INVALIDATE409_12345678901/debtpositions/12345678901IUPDMOCK1/invalidate")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());


	}

}
