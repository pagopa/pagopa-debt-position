package it.gov.pagopa.debtposition.controller;

import static org.assertj.core.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import it.gov.pagopa.debtposition.dto.PaymentOptionDTO;
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
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.mock.DebtPositionMock;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.validation.DebtPositionValidation;

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
	void payPaymentOption_200_with_only_required_receipt_fields() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/PAY_12345678911/debtpositions")
						.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PAY_12345678911/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// effettuo la notifica di pagamento e verifico lo stato in paid
		PaymentOptionDTO data = DebtPositionMock.getPayPOMock1();
		data.setPaymentDate(null);
		data.setPaymentMethod(null);
		data.setFee(0); // because it is a long initialized to 0
		mvc.perform(post("/organizations/PAY_12345678911/paymentoptions/123456IUVMOCK1/pay")
						.content(TestUtil.toJson(data))
						.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.status")
						.value(PaymentOptionStatus.PO_PAID.toString()));

		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/PAY_12345678911/debtpositions/12345678901IUPDMOCK1")
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
		
		// effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico lo stato in paid
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
		
		// effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e verifico lo stato in paid
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
		
		// effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e verifico lo stato in paid
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
		
		// effettuo la notifica di pagamento della seconda rata parziale (setIsPartialPayment = true) e verifico lo stato in paid
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
		
		// effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico lo stato in paid
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
		
		// effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico lo stato in paid
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
		
		// effettuo un nuovo pagamento su una delle rate parziali (setIsPartialPayment = true) per la payment option
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
	void payPaymentOption_422() throws Exception {
		// creo una posizione debitoria (con 'validity date' impostata)
		mvc.perform(post("/organizations/PAY_422_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock6())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata lo stato della posizione debitoria
		mvc.perform(post("/organizations/PAY_422_12345678901/debtpositions/12345678901IUPDMOCK5/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico l'errore 422 di 'Not in payable state' 
		mvc.perform(post("/organizations/PAY_422_12345678901/paymentoptions/123456IUVMOCK6/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isUnprocessableEntity());
	}
	
	@Test
	void payPaymentOption_404() throws Exception {
		// provo a pagare una payment option che non esiste
		String url = "/organizations/PAY_400_12345678901/paymentoptions/123456IUVNOTEXIST";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}
	
	@Test
	void payPaymentOption_400() throws Exception {
		// provo a pagare una payment option con body della request non corretto
		mvc.perform(post("/organizations/PAY_Multiple_Partial2_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPO400Mock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}
	
	/**
	 *  REPORT A TRANSFER
	 */

	@Test
	void reportTransfer_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con una sola PO e isPartialPayment=false
		mvc.perform(post("/organizations/REPORT_200_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/REPORT_200_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento e verifico lo stato in paid
		mvc.perform(post("/organizations/REPORT_200_12345678901/paymentoptions/123456IUVMOCK1/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/REPORT_200_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
		
		// effettuo la rendicontazione per l'unica transazione della PO
		mvc.perform(post("/organizations/REPORT_200_12345678901/paymentoptions/123456IUVMOCK1/transfers/1/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));
		
		//recupero la PO e verifico lo stato in PO_REPORTED
		String url = "/organizations/REPORT_200_12345678901/paymentoptions/123456IUVMOCK1";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_REPORTED.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in reported
		mvc.perform(get("/organizations/REPORT_200_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.REPORTED.toString()));
	}
	
	@Test
	void reportTransfer_Multiple_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/REPORT_Multiple_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/REPORT_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico lo stato in paid
		mvc.perform(post("/organizations/REPORT_Multiple_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/REPORT_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
		
		// effettuo la rendicontazione per l'unica transazione della PO
		mvc.perform(post("/organizations/REPORT_Multiple_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/transfers/3/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));
		
		//recupero la PO e verifico lo stato in PO_REPORTED
		String url = "/organizations/REPORT_Multiple_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_REPORTED.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in reported
		mvc.perform(get("/organizations/REPORT_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.REPORTED.toString()));
	}
	
	@Test
	void reportTransfer_Multiple_Partial_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/REPORT_Multiple_Partial_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/REPORT_Multiple_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e verifico lo stato in paid
		mvc.perform(post("/organizations/REPORT_Multiple_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in partially paid
		mvc.perform(get("/organizations/REPORT_Multiple_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PARTIALLY_PAID.toString()));
		
		// effettuo la rendicontazione per la transazione 
		mvc.perform(post("/organizations/REPORT_Multiple_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/4/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));
		
		//recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
		String url = "/organizations/REPORT_Multiple_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PARTIALLY_REPORTED.toString()));
	}
	
	@Test
	void reportTransfer_Multiple_All_Partial_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e verifico lo stato in paid
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in partially paid
		mvc.perform(get("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PARTIALLY_PAID.toString()));
		
		// effettuo la notifica di pagamento della seconda rata parziale (setIsPartialPayment = true) e verifico lo stato in paid
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK5"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
		mvc.perform(get("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
		
		// effettuo la rendicontazione per una delle 2 transazioni della PO 
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/4/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));

		//recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
		String url = "/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PARTIALLY_REPORTED.toString()));
		
		// effettuo la rendicontazione per la seconda delle 2 transazioni della PO 
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/5/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));

		//recupero la PO e verifico lo stato sia passato in PO_REPORTED
		url = "/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_REPORTED.toString()));
		
		//recupero la PO non ancora rendicontata della posizione debitoria e verifico che sia ancora in PAID
		url = "/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico che lo stato sia ancora in paid
		mvc.perform(get("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
	}
	
	@Test
	void reportTransfer_Multiple_All_Partial_Reported_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e verifico lo stato in paid
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in partially paid
		mvc.perform(get("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PARTIALLY_PAID.toString()));
		
		// effettuo la notifica di pagamento della seconda rata parziale (setIsPartialPayment = true) e verifico lo stato in paid
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK5"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
		mvc.perform(get("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
		
		// effettuo la rendicontazione per una delle 2 transazioni della PO 
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/4/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));

		//recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
		String url = "/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PARTIALLY_REPORTED.toString()));
		
		// effettuo la rendicontazione per la seconda delle 2 transazioni della PO 
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/5/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));

		//recupero la PO e verifico lo stato sia passato in PO_REPORTED
		url = "/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_REPORTED.toString()));
		
		//recupero la PO non ancora rendicontata della posizione debitoria e verifico che sia ancora in PAID
		url = "/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// effettuo la rendicontazione per le 2 transazioni della PO ancora in stato PAID 
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5/transfers/4/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));
		
		mvc.perform(post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5/transfers/5/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));
		
		//recupero la PO e verifico lo stato sia passato in PO_REPORTED
		url = "/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_REPORTED.toString()));
		
		// recupero l'intera posizione debitoria e verifico che lo stato sia passato in reported
		mvc.perform(get("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.REPORTED.toString()));
	}
	
	@Test
	void reportTransfer_409() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con una sola PO e isPartialPayment=false
		mvc.perform(post("/organizations/REPORT_409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/REPORT_409_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/REPORT_409_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()));

		// effettuo la rendicontazione per l'unica transazione della PO ma senza pagamenti in essere 
		mvc.perform(post("/organizations/REPORT_409_12345678901/paymentoptions/123456IUVMOCK1/transfers/1/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
		
	}
	
	@Test
	void reportTransfer_Multiple_409() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/REPORT_Multiple_409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/REPORT_Multiple_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico lo stato in paid
		mvc.perform(post("/organizations/REPORT_Multiple_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));
		
		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/REPORT_Multiple_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
		
		// effettuo la rendicontazione per l'unica transazione della PO
		mvc.perform(post("/organizations/REPORT_Multiple_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/transfers/3/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(TransferStatus.T_REPORTED.toString()));
		
		//recupero la PO e verifico lo stato in PO_REPORTED
		String url = "/organizations/REPORT_Multiple_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_REPORTED.toString()));
		
		//provo a rendicontare nuovamente la transazione già rendicontata
		mvc.perform(post("/organizations/REPORT_Multiple_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/transfers/3/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
	}
	
	@Test
	void reportTransfer_404() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata) con più opzioni di pagamento
		mvc.perform(post("/organizations/REPORT_Multiple_404_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());
		
		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/REPORT_Multiple_404_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico lo stato in paid
		mvc.perform(post("/organizations/REPORT_Multiple_404_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));

		// provo a rendicontare una transazione che non esiste
		mvc.perform(post("/organizations/REPORT_Multiple_404_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/transfers/x/report")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound());
		
	}
	
	/**
	 *  VALIDATION TEST - unexpected case
	 */
	
	@Test
	void ValidationError_checkPaymentPositionOpen() throws Exception {
		try {
			PaymentOption localMockPO = new PaymentOption();
			PaymentPosition localMockPP = new PaymentPosition();
			localMockPP.setStatus(DebtPositionStatus.VALID);
			localMockPO.setStatus(PaymentOptionStatus.PO_PAID);
			localMockPO.setIsPartialPayment(false);
			localMockPP.addPaymentOption(localMockPO);
			DebtPositionValidation.checkPaymentPositionPayability(localMockPP, "mockIUV");
		}
		catch (AppException e) {
			assertTrue(true);
		}
		catch(Exception e) {
			fail("Not the expected exception: "+e.getMessage());
		}
	}
	
	@Test
	void ValidationError_checkPaymentPositionAccountability_PO() throws Exception {
		try {
			PaymentOption localMockPO = new PaymentOption();
			PaymentPosition localMockPP = new PaymentPosition();
			localMockPP.setStatus(DebtPositionStatus.PAID);
			localMockPO.setStatus(PaymentOptionStatus.PO_PAID);
			localMockPO.setIsPartialPayment(false);
			localMockPO.setIuv("iuv");
			localMockPP.addPaymentOption(localMockPO);
			DebtPositionValidation.checkPaymentPositionAccountability(localMockPP, "mockIUV","mockTxID");
		}
		catch (AppException e) {
			assertTrue(true);
		}
		catch(Exception e) {
			fail("Not the expected exception: "+e.getMessage());
		}
	}
	
	@Test
	void ValidationError_checkPaymentPositionAccountability_Transfer() throws Exception {
		try {
			PaymentOption localMockPO = new PaymentOption();
			PaymentPosition localMockPP = new PaymentPosition();
			Transfer localTransfer = new Transfer();
			localMockPP.setStatus(DebtPositionStatus.PAID);
			localMockPO.setStatus(PaymentOptionStatus.PO_PAID);
			localTransfer.setIdTransfer("1");
			localMockPO.addTransfer(localTransfer);
			localMockPO.setIsPartialPayment(false);
			localMockPO.setIuv("mockIUV");
			localMockPP.addPaymentOption(localMockPO);
			DebtPositionValidation.checkPaymentPositionAccountability(localMockPP, "mockIUV","mockTxID");
		}
		catch (AppException e) {
			assertTrue(true);
		}
		catch(Exception e) {
			fail("Not the expected exception: "+e.getMessage());
		}
	}
	
}