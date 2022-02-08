package it.gov.pagopa.debtposition.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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
		// creo una posizione debitoria (con 'validity date' impostata nel futuro)
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

}
