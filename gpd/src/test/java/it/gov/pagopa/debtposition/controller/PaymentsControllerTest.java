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
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
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

	@Test
	void getPaymentOptionByIUV_POPAID_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/POPAID_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/POPAID_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// effettuo la notifica di pagamento e verifico lo stato in paid
		mvc.perform(post("/organizations/POPAID_12345678901/paymentoptions/123456IUVMOCK1/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));

		// recupero la payment option e verifico di nuovo lo stato in paid
		String url = "/organizations/POPAID_12345678901/paymentoptions/123456IUVMOCK1";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv")
				.value("123456IUVMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.amount")
				.value("1000"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.transfer[*]")
				.value(Matchers.hasSize(1)));
	}

	@Test
	void getPaymentOptionByIUV_404() throws Exception {
		String url = "/organizations/PO200_12345678901/paymentoptions/123456IUVNOTEXIST";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	/**
	 *  PAY A PAYMENT OPTION
	 */

	@Test
	void payPaymentOption_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/PAY_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PAY_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento e verifico lo stato in paid
		mvc.perform(post("/organizations/PAY_12345678901/paymentoptions/123456IUVMOCK1/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/PAY_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
	}
	
	@Test
	void payPaymentOption_Multiple_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/PAY_Multiple_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PAY_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento della rata unica (isPartialPayment = false) e verifico lo stato in paid
		mvc.perform(post("/organizations/PAY_Multiple_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/PAY_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
	}
	
	@Test
	void payPaymentOption_Multiple_Partial_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/PAY_Multiple_Partial_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PAY_Multiple_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento di una rata parziale (isPartialPayment = true) e verifico lo stato in paid
		mvc.perform(post("/organizations/PAY_Multiple_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in partially paid
		mvc.perform(get("/organizations/PAY_Multiple_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PARTIALLY_PAID.toString()));
	}
	
	@Test
	void payPaymentOption_Multiple_All_Partial_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/PAY_Multiple_All_Partial_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PAY_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento di una rata parziale (isPartialPayment = true) e verifico lo stato in paid
		mvc.perform(post("/organizations/PAY_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in partially paid
		mvc.perform(get("/organizations/PAY_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PARTIALLY_PAID.toString()));
		
		// effettuo la notifica di pagamento della seconda rata parziale (isPartialPayment = true) e verifico lo stato in paid
		mvc.perform(post("/organizations/PAY_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK5"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
		mvc.perform(get("/organizations/PAY_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
	}
	
	@Test
	void payPaymentOption_Multiple_409() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/PAY_Multiple_409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PAY_Multiple_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento della rata unica (isPartialPayment = false) e verifico lo stato in paid
		mvc.perform(post("/organizations/PAY_Multiple_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/PAY_Multiple_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
		
		// effettuo un nuovo pagamento per la stessa payment option
		mvc.perform(post("/organizations/PAY_Multiple_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
	}
	
	@Test
	void payPaymentOption_Multiple_Partial_409() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/PAY_Multiple_Partial_409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PAY_Multiple_Partial_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento della rata unica (isPartialPayment = false) e verifico lo stato in paid
		mvc.perform(post("/organizations/PAY_Multiple_Partial_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/PAY_Multiple_Partial_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
		
		// effettuo un nuovo pagamento su una delle rate parziali (isPartialPayment = true) per la payment option
		mvc.perform(post("/organizations/PAY_Multiple_Partial_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
	}
	
	@Test
	void payPaymentOption_Multiple_Partial2_409() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/PAY_Multiple_Partial2_409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PAY_Multiple_Partial2_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// effettuo la notifica di pagamento di una rata parziale (isPartialPayment = true) e verifico lo stato in paid
		mvc.perform(post("/organizations/PAY_Multiple_Partial2_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));

		// recupero l'intera posizione debitoria e verifico lo stato in partially paid
		mvc.perform(get("/organizations/PAY_Multiple_Partial2_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PARTIALLY_PAID.toString()));
		
		// effettuo un nuovo pagamento sulla payment option non rateizzabile (isPartialPayment = false) e ottengo errore
		mvc.perform(post("/organizations/PAY_Multiple_Partial2_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
	}
	
	@Test
	void payPaymentOption_404() throws Exception {
		// provo a pagare una payment option che non esiste
		String url = "/organizations/PAY_400_12345678901/paymentoptions/123456IUVNOTEXIST";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}




}
