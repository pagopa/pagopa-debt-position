package it.gov.pagopa.debtposition.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.hamcrest.Matchers;
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
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class PaymentsControllerTest {

	@Autowired
	private MockMvc mvc;

	@Mock
	private ModelMapper modelMapperMock;

	@BeforeEach
	void setUp() {
	}



	/**
	 *  GET PAYMENT OPTION BY IUV
	 */
	@Test
	void getPaymentOptionByIUV_200() throws Exception {
		// creo una posizione debitoria e recupero la payment option associata
		mvc.perform(post("/organizations/PO200_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		String url = "/organizations/PO200_12345678901/paymentoptions/123456IUVMOCK1";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	@Test
	void getPaymentOptionByIUV_MultiplePO_200() throws Exception {
		// creo una posizione debitoria con più payment option associate
		mvc.perform(post("/organizations/PO200_Multiple_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// ne recupero una e verifico sia quella attesa
		String url = "/organizations/PO200_Multiple_12345678901/paymentoptions/123456IUVMULTIPLEMOCK2";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv")
				.value("123456IUVMULTIPLEMOCK2"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.amount")
				.value("500"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_UNPAID.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.transfer[*]")
				.value(Matchers.hasSize(1)));
	}

	// TODO: Appena saranno disponibili le api per simulare un pagamento implementare questo test 
	@Test
	void getPaymentOptionByIUV_POPAID_200() throws Exception {
		// effettuo un pagamento su una posizione debitoria e verifico che lo stato sia passato a PO_PAID
	}
	
	@Test
	void getPaymentOptionByIUV_404() throws Exception {
		String url = "/organizations/PO200_12345678901/paymentoptions/123456IUVNOTEXIST";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}




}
