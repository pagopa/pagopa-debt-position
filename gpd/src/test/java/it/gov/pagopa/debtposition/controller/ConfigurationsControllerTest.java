package it.gov.pagopa.debtposition.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

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

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class ConfigurationsControllerTest {

	@Autowired
	private MockMvc mvc;

	@Mock
	private ModelMapper modelMapperMock;

	@BeforeEach
	void setUp() {
	}
	
	/**
	 *  GET ORGANIZATIONS
	 */
	
	@Test
	void getOrganizations_200() throws Exception {
		
		// creo una posizione debitoria e recupero la payment option associata
		mvc.perform(post("/organizations/GET_ORG_200_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero le organizzazioni inserite a sistema a fino alla data corrente 
		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		mvc.perform(get("/organizations?since="+df.format(LocalDateTime.now(ZoneOffset.UTC)))
				.contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isOk()).andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.add[*]").isNotEmpty())
		.andExpect(MockMvcResultMatchers.jsonPath("$.delete[*]").isEmpty());

	}
	
	/**
	 * CHECK ORGANIZATION
	 */
	
	@Test
	void checkOrganization_200() throws Exception {
		
		// creo una posizione debitoria
		mvc.perform(post("/organizations/CHECK_ORG_200_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// ne verifico l'esistenza 
		mvc.perform(get("/organizations/CHECK_ORG_200_12345678901")
				.contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isOk());
	
	}
	
	@Test
	void checkOrganization_404() throws Exception {
		
		// provo a recuperare un'organizzazione che non esiste 
		mvc.perform(get("/organizations/CHECK_ORG_400_12345678901")
				.contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isNotFound());
	
	}
}