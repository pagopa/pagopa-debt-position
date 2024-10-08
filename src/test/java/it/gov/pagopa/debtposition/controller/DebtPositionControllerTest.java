package it.gov.pagopa.debtposition.controller;

import static org.hamcrest.CoreMatchers.containsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

import it.gov.pagopa.debtposition.dto.*;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import org.apache.commons.lang3.RandomStringUtils;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.client.NodeClient;
import it.gov.pagopa.debtposition.mock.DebtPositionMock;
import it.gov.pagopa.debtposition.model.checkposition.NodeCheckPositionModel;
import it.gov.pagopa.debtposition.model.checkposition.response.NodeCheckPositionResponse;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class DebtPositionControllerTest {

	@Autowired
	private MockMvc mvc;

	@Mock
	private ModelMapper modelMapperMock;
	
	@MockBean private NodeClient nodeClient;
	
	@Value("${nav.aux.digit}")
    private String auxDigit;

	@BeforeEach
	void setUp() {
	}

	/**
	 * CREATE DEBT POSITION
	 */
	@Test
	void createDebtPosition_201() throws Exception {
		mvc.perform(post("/organizations/12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].iuv")
				.value("123456IUVMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value(auxDigit+"123456IUVMOCK1"));
	}

	@Test
	void createDebtPosition_blank_input_201() throws Exception {
		PaymentPositionDTO mock1 = DebtPositionMock.getMock1();
		mock1.setFullName(" ");
		mock1.setFiscalCode(" ");
		mvc.perform(post("/organizations/12345678901/debtpositions")
						.content(TestUtil.toJson(mock1)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].iuv")
						.value("123456IUVMOCK1"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
						.value(auxDigit+"123456IUVMOCK1"));
	}

	@Test
	void createDebtPosition_null_input_400() throws Exception {
		PaymentPositionDTO mock1 = DebtPositionMock.getMock1();
		mock1.setFullName(null);
		mock1.setFiscalCode(null);
		mvc.perform(post("/organizations/12345678901/debtpositions")
						.content(TestUtil.toJson(mock1)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isBadRequest())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	@Test
	void createDebtPositionWithStamp_201() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		TransferDTO t = new TransferDTO(pp.getOrganizationFiscalCode(), "1", pp.getPaymentOption().get(0).getAmount(), "info", "0", "", "",
				new Stamp("hash1", "01", "ML"), TransferStatus.T_UNREPORTED);
		pp.getPaymentOption().get(0).getTransfer().set(0, t);
		pp.setIupd(RandomStringUtils.randomNumeric(20));
		pp.getPaymentOption().get(0).setIuv(RandomStringUtils.randomNumeric(17));
		mvc.perform(post("/organizations/12345678901/debtpositions")
							.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.hashDocument")
								   .value(t.getStamp().getHashDocument()))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.stampType")
								   .value(t.getStamp().getStampType()))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.provincialResidence")
									.value(t.getStamp().getProvincialResidence()));
	}

	@Test
	void createDebtPosition_Multiple_201() throws Exception {
		// creazione di due posizione debitorie per la stessa organizzazione (2 PaymentOption)
		mvc.perform(post("/organizations/12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value("3123456IUVMULTIPLEMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].nav")
				.value("3123456IUVMULTIPLEMOCK2"));

		mvc.perform(post("/organizations/12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value("3123456IUVMULTIPLEMOCK3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].nav")
				.value("3123456IUVMULTIPLEMOCK4"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[2].nav")
				.value("3123456IUVMULTIPLEMOCK5"));
	}
	
	@Test
	void createDebtPosition_Custom_NAV_201() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		pp.getPaymentOption().forEach(po -> po.setNav("CUSTOM_"+auxDigit+po.getIuv()));
		
		mvc.perform(post("/organizations/12345678901_NAV/debtpositions")
				.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value("CUSTOM_"+auxDigit+"123456IUVMOCK1"));
	}

	@Test
	void createDebtPosition_transferToDifferentCI_200() throws Exception {

		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
		paymentPositionDTO.getPaymentOption().get(0).getTransfer().get(0).setOrganizationFiscalCode("ANOTHERCI123");

		mvc.perform(post("/organizations/12345678901200_transferToDifferentCI/debtpositions")
						.content(TestUtil.toJson(paymentPositionDTO)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		// recupero la posizione debitoria e controllo i valori dei fiscal code degli EC
		String url = "/organizations/12345678901200_transferToDifferentCI/debtpositions/12345678901IUPDMOCK1";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.organizationFiscalCode")
						.value("12345678901200_transferToDifferentCI"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].organizationFiscalCode")
						.value("12345678901200_transferToDifferentCI"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].organizationFiscalCode")
						.value("ANOTHERCI123"));
	}

	@Test
	void createDebtPosition_400() throws Exception {
		mvc.perform(post("/organizations/400_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.get400Mock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON));

		mvc.perform(post("/organizations/400_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.get400Mock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	@Test
	void createDebtPosition_Country_400() throws Exception {
		// provo a creare una posizione debitoria dove il country non rispetti la regexp [A-Z]{2}
		mvc.perform(post("/organizations/400_12345678901/debtpositions")
						.content(TestUtil.toJson(DebtPositionMock.get400Mock9())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}
	
	@Test
	void createDebtPosition_Amount_400() throws Exception {
		// provo a creare una posizione debitoria dove l'amount previsto per la PO differisce da quello del transfer
		mvc.perform(post("/organizations/400_Amount_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.get400Mock4())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON));

	}
	
	@Test
	void createDebtPosition_Retention_400() throws Exception {
		// provo a creare una posizione debitoria dove la retention date è minore della due_date
		mvc.perform(post("/organizations/400_Retention_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.get400Mock5())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON));

	}
	
	@Test
	void createDebtPosition_Validity_400() throws Exception {
		// provo a creare una posizione debitoria dove la retention date è minore della due_date
		mvc.perform(post("/organizations/400_Validity_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.get400Mock6())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON));

	}
	
	@Test
	void createDebtPosition_Num_Transfers_400() throws Exception {
		// provo a creare una posizione debitoria dove il numero di trasfers supera il massimo di 5
		mvc.perform(post("/organizations/400_Num_Transfers_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.get400Mock7())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON));

	}
	
	@Test
	void createDebtPosition_ID_Transfer_400() throws Exception {
		// provo a creare una posizione debitoria dove l'id_transfer non è compreso tra 1 e 5
		mvc.perform(post("/organizations/400_ID_Transfer_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.get400Mock8())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON));

	}

	@Test
	void createDebtPosition_409() throws Exception {
		mvc.perform(post("/organizations/409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated()).andExpect(content().contentType(MediaType.APPLICATION_JSON));

		// provo a creare 2 posizioni debitorie con lo stesso organization_fiscal_code
		// => la seconda chiamata deve andare in errore con codice 409
		mvc.perform(post("/organizations/409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isConflict()).andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}
	
	@Test
	void createDebtPosition_Custom_NAV_409() throws Exception {
		PaymentPositionDTO ppNav = DebtPositionMock.getMock1();
		ppNav.getPaymentOption().forEach(po -> po.setNav("CUSTOM_"+auxDigit+po.getIuv()));
		
		mvc.perform(post("/organizations/409_12345678901_NAV/debtpositions")
				.content(TestUtil.toJson(ppNav)).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value("CUSTOM_"+auxDigit+"123456IUVMOCK1"));
		
		// provo a creare una seconda posizione debitoria per la stessa organizzazione, cambiando lo IUPD e lo IUV ma non il NAV 
	    // => la chiamata deve andare in errore con codice 409 (violazione unique constraint)
		ppNav.setIupd((int)(Math.random()*100)+"_"+ppNav.getIupd());
		ppNav.getPaymentOption().forEach(po -> po.setIuv((int)(Math.random()*100)+"_"+po.getIuv()));
		mvc.perform(post("/organizations/409_12345678901_NAV/debtpositions")
				.content(TestUtil.toJson(ppNav)).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isConflict()).andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	@Test
	void createAndPublishDebtPosition_201() throws Exception {
		//Creo e pubblico la posizione debitoria
		mvc.perform(post("/organizations/CRTPUB_12345678901/debtpositions?toPublish=True")
						.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated()).andExpect(content().contentType(MediaType.APPLICATION_JSON));

		// verifico che lo stato sia stato settato a valid
		mvc.perform(get("/organizations/CRTPUB_12345678901/debtpositions/12345678901IUPDMOCK1")
						.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.status")
						.value(DebtPositionStatus.VALID.toString()))
				.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

		// provo a fare una nuova pubblicazione su una posizione debitoria con uno stato non più idoneo
		mvc.perform(post("/organizations/CRTPUB_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
	}

	@Test
	void createDebtPositionAuthorizedBySegregationCodes_201() throws Exception {
		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
		String validSegregationCode = paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0,2);
		String anotherSegregationCode = "99";
		mvc.perform(post("/organizations/SC_12345678901/debtpositions?segregationCodes=" + validSegregationCode + "," + anotherSegregationCode)
							.content(TestUtil.toJson(paymentPositionDTO)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
								   .value("3123456IUVMOCK1"));
	}

	@Test
	void createDebtPositionAuthorizedBySegregationCodes_403() throws Exception {
		String notSufficientSegregationCode = "99";
		mvc.perform(post("/organizations/12345678901/debtpositions?segregationCodes=" + notSufficientSegregationCode)
							.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isForbidden());
	}
	
	@Test
	void createDebtPositionWithMetadata_201() throws Exception {
		// creo una posizione debitoria con metadati su PO e transfer
		mvc.perform(post("/organizations/12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMetadataMock8())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value("3123456IUVMETADATAMOCK9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key").value("keypometadatamock9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key").value("keytransfermetadatamock3"));
				
	}
	
	@Test
	void createDebtPositionWithMetadata_400() throws Exception {
		// creo una posizione debitoria con più di 10 occorrenze di metadati (numero massimo accettato) --> request rifiutata
		PaymentPositionDTO pp = DebtPositionMock.getMetadataMock8();
		PaymentOptionDTO po = pp.getPaymentOption().get(0);
		for (int i=0; i<=10; i++) {
			po.addPaymentOptionMetadata(PaymentOptionMetadataDTO.builder().key("key"+i).value("value"+i).build());
		}
		System.out.println(TestUtil.toJson(pp));
		mvc.perform(post("/organizations/12345678901/debtpositions")
				.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isBadRequest())
		.andExpect(content().string(containsString("size must be between 0 and 10")));
	}

	/**
	 *  GET DEBT POSITION BY IUV
	 */
	@Test
	void getDebtPositionByIUV_200() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		pp.setIupd(RandomStringUtils.randomNumeric(20));
		String iuv = RandomStringUtils.randomNumeric(17);
		pp.getPaymentOption().get(0).setIuv(iuv);
		mvc.perform(post("/organizations/20077777771/debtpositions")
						.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		String url = "/organizations/20077777771/paymentoptions/" + iuv + "/debtposition";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
						.value(auxDigit+iuv));
	}

	@Test
	void getDebtPositionWithStampByIUV_200() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		TransferDTO t = new TransferDTO(pp.getOrganizationFiscalCode(), "1", pp.getPaymentOption().get(0).getAmount(), "info", "0", "", "",
				new Stamp("hash1", "01", "ML"), TransferStatus.T_UNREPORTED);
		pp.getPaymentOption().get(0).getTransfer().set(0, t);
		String iuv = RandomStringUtils.randomNumeric(17);
		pp.getPaymentOption().get(0).setIuv(iuv);
		pp.setIupd(RandomStringUtils.randomNumeric(20));
		mvc.perform(post("/organizations/20077777771/debtpositions")
							.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());
		String url = "/organizations/20077777771/paymentoptions/" + iuv + "/debtposition";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.hashDocument")
								   .value(t.getStamp().getHashDocument()))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.stampType")
								   .value(t.getStamp().getStampType()))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.provincialResidence")
								   .value(t.getStamp().getProvincialResidence()));
	}

	@Test
	void getDebtPositionByIUV_WithMetadata_200() throws Exception {
		// Creo una posizione debitoria con metadati su PO e transfer, la recupero e verifico siano presenti i metadati inseriti
		PaymentPositionDTO pp = DebtPositionMock.getMetadataMock8();
		String iuv = "47999999999999999";
		pp.getPaymentOption().get(0).setIuv(iuv);
		mvc.perform(post("/organizations/20077777772/debtpositions")
						.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated()).andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value(auxDigit+iuv))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
						.value("keypometadatamock9"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
						.value("keytransfermetadatamock3"));

		String url = "/organizations/20077777772/paymentoptions/" + iuv + "/debtposition";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value(auxDigit+iuv))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
						.value("keypometadatamock9"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
						.value("keytransfermetadatamock3"));
	}

	@Test
	void getDebtPositionByIUV_Custom_NAV_200() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		String iuv = "47999999999999999";
		pp.getPaymentOption().get(0).setIuv(iuv);
		pp.getPaymentOption().forEach(po -> po.setNav("9"+auxDigit+po.getIuv()));

		// Creo una posizione debitoria settando il NAV e la recupero
		mvc.perform(post("/organizations/20077777773/debtpositions")
						.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
						.value("9"+auxDigit+iuv));
		String url = "/organizations/20077777773/paymentoptions/" + iuv + "/debtposition";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
						.value("9"+auxDigit+iuv));
	}

	@Test
	void getDebtPositionByIUV_SegregationCodeAuthorized_200() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		String iuv = "47999999999999999";
		pp.getPaymentOption().get(0).setIuv(iuv);
		String validSegregationCode = pp.getPaymentOption().get(0).getIuv().substring(0,2);
		String anotherSegregationCode = "99";
		// creo una posizione debitoria e la recupero
		mvc.perform(post("/organizations/20077777774/debtpositions")
						.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		String url = "/organizations/20077777774/paymentoptions/" + iuv + "/debtposition?segregationCodes=" + validSegregationCode + "," + anotherSegregationCode;
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
						.value(auxDigit+iuv));
	}

	@Test
	void getDebtPositionByIUV_SegregationCodeForbidden_403() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		String iuv = "47999999999999999";
		pp.getPaymentOption().get(0).setIuv(iuv);
		String notSufficientSegregationCode = "99";
		mvc.perform(post("/organizations/40377777771/debtpositions")
						.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());
		String url = "/organizations/40377777771/paymentoptions/" + iuv + "/debtposition?segregationCodes=" + notSufficientSegregationCode;
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isForbidden());
	}

	@Test
	void getDebtPositionByIUV_404() throws Exception {
		String NOT_EXISTENT_IUV = "00999999999999999";
		String url = "/organizations/40377777771/paymentoptions/" + NOT_EXISTENT_IUV + "/debtposition";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	/**
	 *  GET DEBT POSITION BY IUPD
	 */
	@Test
	void getDebtPositionByIUPD_200() throws Exception {
		// creo una posizione debitoria e la recupero
		mvc.perform(post("/organizations/200_12345678901/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		String url = "/organizations/200_12345678901/debtpositions/12345678901IUPDMOCK1";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
								   .value(auxDigit+"123456IUVMOCK1"));
	}

	@Test
	void getDebtPositionWithStampByIUPD_200() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		TransferDTO t = new TransferDTO(pp.getOrganizationFiscalCode(), "1", pp.getPaymentOption().get(0).getAmount(), "info", "0", "", "",
				new Stamp("hash1", "01", "ML"), TransferStatus.T_UNREPORTED);
		pp.getPaymentOption().get(0).getTransfer().set(0, t);
		mvc.perform(post("/organizations/20077777771/debtpositions")
							.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		String url = "/organizations/20077777771/debtpositions/" + pp.getIupd();
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.hashDocument")
								   .value(t.getStamp().getHashDocument()))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.stampType")
								   .value(t.getStamp().getStampType()))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.provincialResidence")
								   .value(t.getStamp().getProvincialResidence()));
	}
	
	@Test
	void getDebtPositionByIUPDWithMetadata_200() throws Exception {
		// creo una posizione debitoria con metadati su PO e transfer, la recupero e verifico siano presenti i metadati inseriti
		mvc.perform(post("/organizations/200_metadata_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMetadataMock8())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated()).andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("3123456IUVMETADATAMOCK9"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
						.value("keypometadatamock9"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
						.value("keytransfermetadatamock3"));

		String url = "/organizations/200_metadata_12345678901/debtpositions/12345678901IUPDMETADATAMOCK7";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("3123456IUVMETADATAMOCK9"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
						.value("keypometadatamock9"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
						.value("keytransfermetadatamock3"));
	}
	
	@Test
	void getDebtPositionByIUPD_Custom_NAV_200() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		pp.getPaymentOption().forEach(po -> po.setNav("CUSTOM_"+auxDigit+po.getIuv()));
		
		// creo una posizione debitoria settando il NAV e la recupero
		mvc.perform(post("/organizations/200_12345678901_NAV/debtpositions")
				.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value("CUSTOM_"+auxDigit+"123456IUVMOCK1"));
		
		String url = "/organizations/200_12345678901_NAV/debtpositions/12345678901IUPDMOCK1";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
								   .value("CUSTOM_"+auxDigit+"123456IUVMOCK1"));
	}

	@Test
	void getDebtPositionByIUPD_SegregationCodeAuthorized_200() throws Exception {
		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
		String validSegregationCode = paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0,2);
		String anotherSegregationCode = "99";
		// creo una posizione debitoria e la recupero
		mvc.perform(post("/organizations/200_SC_12345678901/debtpositions")
							.content(TestUtil.toJson(paymentPositionDTO)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		String url = "/organizations/200_SC_12345678901/debtpositions/12345678901IUPDMOCK1?segregationCodes=" + validSegregationCode + "," + anotherSegregationCode;
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
								   .value("3123456IUVMOCK1"));
	}

	@Test
	void getDebtPositionByIUPD_SegregationCodeForbidden_403() throws Exception {
		String notSufficientSegregationCode = "99";
		// creo una posizione debitoria e la recupero
		mvc.perform(post("/organizations/403_SC_12345678901/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		String url = "/organizations/200_SC_12345678901/debtpositions/12345678901IUPDMOCK1?segregationCodes=" + notSufficientSegregationCode;
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isForbidden());
	}
	
	@Test
	void getDebtPositionByIUPD_404() throws Exception {
		String url = "/organizations/200_12345678901/debtpositions/12345678901IUPDNOTEXIST";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	/**
	 *  GET LIST DEBT POSITIONS
	 */
	@Test
	void getDebtPositionList() throws Exception {
		// creo due posizioni debitorie e recupero tutte le payment_option di entrambe
		mvc.perform(post("/organizations/LIST_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(post("/organizations/LIST_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST_12345678901/debtpositions?page=0" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS))
				+ "&orderby=IUPD&ordering=ASC";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				.value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
				.value(Matchers.hasSize(3)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[0].nav")
				.value("3123456IUVMULTIPLEMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[1].nav")
				.value("3123456IUVMULTIPLEMOCK2"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[0].nav")
				.value("3123456IUVMULTIPLEMOCK3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[1].nav")
				.value("3123456IUVMULTIPLEMOCK4"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[2].nav")
				.value("3123456IUVMULTIPLEMOCK5"));
	}
	
	@Test
	void getDebtPositionList_NAV() throws Exception {
		
		// creo due posizioni debitorie di cui una con il NAV settato in fase di creazione e l'altra con il default <AUX_DIGIT>+IUV
		PaymentPositionDTO ppNav = DebtPositionMock.getMock2();
		ppNav.getPaymentOption().forEach(po -> po.setNav("CUSTOM_"+auxDigit+po.getIuv()));
		mvc.perform(post("/organizations/LIST_NAV_12345678901/debtpositions")
				.content(TestUtil.toJson(ppNav)).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(post("/organizations/LIST_NAV_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST_NAV_12345678901/debtpositions?page=0" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS))
				+ "&orderby=IUPD&ordering=ASC";
		
		// recupero le payment_option di entrambe e verifico che una abbia il NAV custom e l'altra in NAV di default
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				.value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
				.value(Matchers.hasSize(3)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[0].nav")
				.value("CUSTOM_"+auxDigit+"123456IUVMULTIPLEMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[1].nav")
				.value("CUSTOM_"+auxDigit+"123456IUVMULTIPLEMOCK2"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[0].nav")
				.value(auxDigit+"123456IUVMULTIPLEMOCK3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[1].nav")
				.value(auxDigit+"123456IUVMULTIPLEMOCK4"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[2].nav")
				.value(auxDigit+"123456IUVMULTIPLEMOCK5"));
	}

	@Test
	void getDebtPositionList_UpdateDateInterval() throws Exception {
		// creo due posizioni debitorie e recupero tutte le payment_option di entrambe, inserendo nel filtro solo la due_date_from
		mvc.perform(post("/organizations/LIST_12345678904/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		mvc.perform(post("/organizations/LIST_12345678904/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST_12345678904/debtpositions?page=0" + "&due_date_from="
							 + df.format(LocalDateTime.now(ZoneOffset.UTC))
							 + "&orderby=IUPD&ordering=ASC";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
								   .value(Matchers.hasSize(2)))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
								   .value(Matchers.hasSize(3)));
	}

	@Test
	void getDebtPositionListByPaymentDate() throws Exception {
		// creo la posizione debitoria DRAFT
		mvc.perform(post("/organizations/LIST_12345678902/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		// creo la posizione debitoria (senza 'validity date' impostata) che dopo il pagamento sarà PAID
		mvc.perform(post("/organizations/LIST_12345678902/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/LIST_12345678902/debtpositions/12345678901IUPDMOCK1/publish")
							.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// effettuo la notifica di pagamento
		mvc.perform(post("/organizations/LIST_12345678902/paymentoptions/"+auxDigit+"123456IUVMOCK1/pay")
							.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
							.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON));

		// effettuo la chiamata GET applicando il filtro sulla payment_date
		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST_12345678902/debtpositions?page=0" +
							 "&payment_date_from=" + df.format(LocalDateTime.now(ZoneOffset.UTC)) +
							 "&payment_date_to=" + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list")
								   .value(Matchers.hasSize(1)));
	}

	@Test
	void getDebtPositionListByStatus() throws Exception {
		// creo la posizione debitoria DRAFT
		mvc.perform(post("/organizations/LIST_12345678903/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		// creo la posizione debitoria (senza 'validity date' impostata) che sarà PAID dopo il pagamento
		mvc.perform(post("/organizations/LIST_12345678903/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/LIST_12345678903/debtpositions/12345678901IUPDMOCK1/publish")
							.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// effettuo la notifica di pagamento
		mvc.perform(post("/organizations/LIST_12345678903/paymentoptions/"+auxDigit+"123456IUVMOCK1/pay")
							.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
							.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// effettuo la chiamata GET applicando il filtro sullo status
		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST_12345678903/debtpositions?page=0" + "&due_date_from="
							 + df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&due_date_to="
							 + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS))
							 + "&status=PAID";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list")
								   .value(Matchers.hasSize(1)));
	}
	
	@Test
	void getDebtPositionListOrdered() throws Exception {
		// creo due posizioni debitorie e le recupero con ordinamento
		mvc.perform(post("/organizations/LIST_ORDERED_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(post("/organizations/LIST_ORDERED_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST_ORDERED_12345678901/debtpositions?page=0&orderby=INSERTED_DATE&ordering=DESC" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].iupd").value("12345678901IUPDMULTIPLEMOCK2"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].iupd").value("12345678901IUPDMULTIPLEMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				.value(Matchers.hasSize(3)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
				.value(Matchers.hasSize(2)));
	}

	@Test
	void getDebtPositionList_partial() throws Exception {
		// creo due posizioni debitorie ed estraggo per intervallo di date che non comprende tutte le payment_option create
		mvc.perform(post("/organizations/DUEDATEBETWEEN_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(post("/organizations/DUEDATEBETWEEN_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/DUEDATEBETWEEN_12345678901/debtpositions?page=0" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.DAYS))
				+ "&orderby=IUPD&ordering=ASC";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				// manca la payment_option che ha una due_date maggiore di quella inserita nella ricerca
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
				.value(Matchers.hasSize(3)));
	}

	@Test
	void getDebtPositionList_404() throws Exception {
		// provo a recuperare una posizione debitoria con una url sbagliata
		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST404_12345678901/debtpositions" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound());
	}

	@Test
	void getDebtPositionListDueDate_400() throws Exception {
		// provo a recuperare una posizione debitoria passando le date con un formato diverso da quello atteso
		DateTimeFormatter df = DateTimeFormatter.ofPattern("dd-MM-yyyy");
		String url = "/organizations/LIST404_12345678901/debtpositions?page=0" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}
	
	@Test
	void getDebtPositionListDueDate_Interval_400() throws Exception {
		// provo a recuperare una posizione debitoria passando un intervallo di date troppo ampio 
		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST404_12345678901/debtpositions?page=0" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(60, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}
	
	@Test
	void getDebtPositionListDueDate_InversionDate_400() throws Exception {
		// provo a recuperare una posizione debitoria passando un intervallo di date invertito (from > to) 
		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST404_12345678901/debtpositions?page=0" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(60, ChronoUnit.DAYS)) + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	@Test
	void getDebtPositionList_MutualExclusion_400() throws Exception {
		// provo a recuperare una posizione debitoria passando sia l'intervallo di date due_date che payment_date
		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/LIST404_12345678901/debtpositions?page=0" + "&due_date_from="
							 + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS)) + "&due_date_to="
							 + df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&payment_date_from="
							 + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS)) + "&payment_date_to="
							 + df.format(LocalDateTime.now(ZoneOffset.UTC));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	@Test
	void getDebtPositionListBySegregationCode_200() throws Exception {
		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock2();
		String iuv1 = paymentPositionDTO.getPaymentOption().get(0).getIuv();
		String iuv2 = paymentPositionDTO.getPaymentOption().get(1).getIuv();
		String firstSegregationCode = paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0,2);
		// Create 2 DEBT POSITION but GET only the one related to the given segregation code because the caller is authorized only for the first
		mvc.perform(post("/organizations/LIST_SC_12345678901/debtpositions")
							.content(TestUtil.toJson(paymentPositionDTO)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		String url = "/organizations/LIST_SC_12345678901/debtpositions?page=0&segregationCodes=" + firstSegregationCode;
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(1)))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
								   .value(Matchers.hasSize(2)))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[0].nav")
								   .value("3"+iuv1))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[1].nav")
								   .value("3"+iuv2));
	}

	@Test
	void getDebtPositionListBySegregationCode_403() throws Exception {
		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock2();
		String firstSegregationCode = "99";
		// Create 2 DEBT POSITION but GET only the one related to the given segregation code because the caller is authorized only for the first
		mvc.perform(post("/organizations/LIST_403_SC_12345678901/debtpositions")
							.content(TestUtil.toJson(paymentPositionDTO)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		mvc.perform(post("/organizations/LIST_403_SC_12345678901/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		String url = "/organizations/LIST_403_SC_12345678901/debtpositions?page=0&segregationCodes=" + firstSegregationCode;
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(0)));
	}

	/**
	 * DELETE DEBT POSITION
	 */
	@Test
	void deleteDebtPosition_200() throws Exception {
		// creo una posizione debitoria e la cancello
		mvc.perform(post("/organizations/DEL_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(delete("/organizations/DEL_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isOk());
	}

	@Test
	void deleteDebtPosition_404() throws Exception {
		// provo a cancellare una posizione debitoria che non esiste
		mvc.perform(delete("/organizations/DEL_12345678901/debtpositions/12345678901IUPDNOTEXIST")
				.contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isNotFound());
	}

	@Test
	void deleteDebtPosition_409() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/DEL_409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/DEL_409_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// effettuo la notifica di pagamento e verifico lo stato in paid
		mvc.perform(post("/organizations/DEL_409_12345678901/paymentoptions/"+auxDigit+"123456IUVMOCK1/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));

		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/DEL_409_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));
		
		// provo a cancellare la posizione debitoria che ha già un pagamento in essere
		mvc.perform(delete("/organizations/DEL_409_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isConflict());
	}

	@Test
	void deleteDebtPosition_SegregationCode_200() throws Exception {
		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
		String segregationCode = paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0,2);
		mvc.perform(post("/organizations/DEL_12345678901/debtpositions")
							.content(TestUtil.toJson(paymentPositionDTO)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		mvc.perform(delete("/organizations/DEL_12345678901/debtpositions/12345678901IUPDMOCK1?segregationCodes=" + segregationCode)
							.contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isOk());
	}

	@Test
	void deleteDebtPosition_SegregationCode_403() throws Exception {
		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
		String segregationCode = "99";
		mvc.perform(post("/organizations/DEL_12345678901/debtpositions")
							.content(TestUtil.toJson(paymentPositionDTO)).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		mvc.perform(delete("/organizations/DEL_12345678901/debtpositions/12345678901IUPDMOCK1?segregationCodes=" + segregationCode)
							.contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isForbidden());
	}

	/**
	 * UPDATE DEBT POSITION
	 */
	@Test
	void updateDebtPosition_200() throws Exception {
		// creo una posizione debitoria 
		mvc.perform(post("/organizations/UPD_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value(auxDigit+"123456IUVMOCK1"));

		// recupero la posizione debitoria e verifico il contenuto
		mvc.perform(get("/organizations/UPD_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.companyName")
				.value("Comune di Firenze"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv")
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value(auxDigit+"123456IUVMOCK1"));

		// aggiorno la posizione debitoria
		mvc.perform(put("/organizations/UPD_12345678901/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(DebtPositionMock.getMock4()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che il nuovo contenuto 
		mvc.perform(get("/organizations/UPD_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.companyName")
				.value("Comune di Roma"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv")
				.value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].amount")
				.value(500))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].transfer[0].amount")
				.value(500))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value(auxDigit+"123456IUVMULTIPLEMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].nav")
				.value(auxDigit+"123456IUVMULTIPLEMOCK2"));
	}
	
	@Test
	void updateDebtPositionWithMetadata_200() throws Exception {
		// creo una posizione debitoria con metadati su PO e transfer
		mvc.perform(post("/organizations/200_UPD_metadata_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMetadataMock8())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated()).andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("3123456IUVMETADATAMOCK9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
				.value("keypometadatamock9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
				.value("keytransfermetadatamock3"));

		//recupero la posizione debitoria e verifico siano presenti i metadati inseriti
		String url = "/organizations/200_UPD_metadata_12345678901/debtpositions/12345678901IUPDMETADATAMOCK7";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("3123456IUVMETADATAMOCK9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
				.value("keypometadatamock9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
				.value("keytransfermetadatamock3"));

		// aggiorno la posizione debitoria aggiungendo dei metadati
		PaymentPositionDTO ppToUpdate = DebtPositionMock.getMetadataMock8();
		ppToUpdate.getPaymentOption().get(0).addPaymentOptionMetadata(PaymentOptionMetadataDTO.builder().key("keypometadataupd").build());
		ppToUpdate.getPaymentOption().get(0).getTransfer().get(0).addTransferMetadata(TransferMetadataDTO.builder().key("keytransfermetadataupd").build());
		mvc.perform(put("/organizations/200_UPD_metadata_12345678901/debtpositions/12345678901IUPDMETADATAMOCK7")
				.content(TestUtil.toJson(ppToUpdate))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("3123456IUVMETADATAMOCK9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
				.value("keypometadatamock9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
				.value("keytransfermetadatamock3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[1].key")
				.value("keypometadataupd"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[1].key")
				.value("keytransfermetadataupd"));

		//recupero la posizione debitoria e verifico siano presenti i metadati inseriti
		url = "/organizations/200_UPD_metadata_12345678901/debtpositions/12345678901IUPDMETADATAMOCK7";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("3123456IUVMETADATAMOCK9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
				.value("keypometadatamock9"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
				.value("keytransfermetadatamock3"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[1].key")
				.value("keypometadataupd"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[1].key")
				.value("keytransfermetadataupd"));
	}

	@Test
	void updateDebtPosition_SegregationCode_200() throws Exception {
		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
		String iupd = paymentPositionDTO.getIupd();
		String iuv = paymentPositionDTO.getPaymentOption().get(0).getIuv();
		String segregationCode = paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0,2);
		mvc.perform(post("/organizations/UPD_SC_12345678901/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
								   .value('3'+iuv));

		// aggiorno la posizione debitoria
		mvc.perform(put("/organizations/UPD_SC_12345678901/debtpositions/" + iupd
								+ "?segregationCodes=" + segregationCode)
							.content(TestUtil.toJson(DebtPositionMock.getMock4()))
							.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che il nuovo contenuto
		mvc.perform(get("/organizations/UPD_SC_12345678901/debtpositions/" + iupd)
							.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.companyName")
								   .value("Comune di Roma"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv")
								   .value(Matchers.hasSize(2)))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount")
								   .value(1000))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount")
								   .value(1000))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].amount")
								   .value(500))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].transfer[0].amount")
								   .value(500))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
								   .value("3123456IUVMULTIPLEMOCK1"))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].nav")
								   .value("3123456IUVMULTIPLEMOCK2"));
	}
	
	@Test
	void updateDebtPosition_NAV_200() throws Exception {
		// creo una posizione debitoria con un default NAV <AUX_DIGIT>+IUV
		mvc.perform(post("/organizations/UPD_NAV_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value(auxDigit+"123456IUVMOCK1"));

		// recupero la posizione debitoria e verifico il contenuto
		mvc.perform(get("/organizations/UPD_NAV_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.companyName")
				.value("Comune di Firenze"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv")
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value(auxDigit+"123456IUVMOCK1"));

		// aggiorno la posizione debitoria con un custom NAV
		PaymentPositionDTO ppNav = DebtPositionMock.getMock1();
		ppNav.getPaymentOption().forEach(po -> po.setNav("CUSTOM_"+auxDigit+po.getIuv()));
		mvc.perform(put("/organizations/UPD_NAV_12345678901/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(ppNav))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che il nuovo contenuto 
		mvc.perform(get("/organizations/UPD_NAV_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.companyName")
				.value("Comune di Firenze"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv")
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
				.value("CUSTOM_"+auxDigit+"123456IUVMOCK1"));
	}
	
	@Test
	void updateDebtPosition_change_officeName_200() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		
		// creo una posizione debitoria 
		mvc.perform(post("/organizations/UPD_office_12345678901/debtpositions")
				.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico il contenuto
		mvc.perform(get("/organizations/UPD_office_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.companyName")
				.value("Comune di Firenze"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.officeName")
				.value("Ufficio tributario"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv")
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount")
				.value(1000));
		
		

		// aggiorno il nome dell'ufficio per la posizione debitoria
		pp.setOfficeName("Agenzia delle entrate");
		mvc.perform(put("/organizations/UPD_office_12345678901/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(pp))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che l'aggiornamento dell'ufficio sia avvenuto correttamente 
		mvc.perform(get("/organizations/UPD_office_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.companyName")
				.value("Comune di Firenze"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.officeName")
				.value("Agenzia delle entrate"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv")
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount")
				.value(1000));
	}
	
	@Test
	void updateDebtPosition_change_fee_200() throws Exception {
		PaymentPositionDTO pp = DebtPositionMock.getMock1();
		
		// creo una posizione debitoria 
		mvc.perform(post("/organizations/UPD_fee_12345678901/debtpositions")
				.content(TestUtil.toJson(pp)).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico il contenuto
		mvc.perform(get("/organizations/UPD_fee_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.companyName")
				.value("Comune di Firenze"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.officeName")
				.value("Ufficio tributario"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv")
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].fee")
				.value(0));
		
		

		// aggiorno la fee per la posizione debitoria
		pp.getPaymentOption().get(0).setFee(500L);
		mvc.perform(put("/organizations/UPD_fee_12345678901/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(pp))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che l'aggiornamento della fee sia avvenuto correttamente 
		mvc.perform(get("/organizations/UPD_fee_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.companyName")
				.value("Comune di Firenze"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.officeName")
				.value("Ufficio tributario"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv")
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount")
				.value(1000))
		.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].fee")
				.value(500));
	}
	
	@Test
	void updateDebtPosition_Published_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/UPD_PBH_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico lo stato in draft
		mvc.perform(get("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.DRAFT.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato aggiornato a valid (doppio passaggio di stato) 
		mvc.perform(get("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());
		
		// aggiorno la posizione debitoria
		mvc.perform(put("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(DebtPositionMock.getMock4()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
		
		// recupero la posizione debitoria e verifico che lo stato sia tornato in draft
		mvc.perform(get("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.DRAFT.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

	}

	@Test
	void updateDebtPosition_CreateAndPublished_200() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/MRDPLL54H17D542L/debtpositions?toPublish=True")
						.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico lo stato in valid
		mvc.perform(get("/organizations/MRDPLL54H17D542L/debtpositions/12345678901IUPDMOCK1")
						.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.status")
						.value(DebtPositionStatus.VALID.toString()));

		// aggiorno la posizione debitoria
		mvc.perform(put("/organizations/MRDPLL54H17D542L/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(DebtPositionMock.getMock4()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// recupero la posizione debitoria e verifico che lo stato sia tornato in draft
		mvc.perform(get("/organizations/MRDPLL54H17D542L/debtpositions/12345678901IUPDMOCK1")
						.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.status")
						.value(DebtPositionStatus.DRAFT.toString()))
				.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

	}
	
	@Test
	void updateAndPublishDebtPosition_200() throws Exception {
	  // creo una posizione debitoria (senza 'validity date' impostata)
	  mvc.perform(post("/organizations/UPDANDPBH_12345678901/debtpositions")
	      .content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isCreated());

	  // recupero la posizione debitoria e verifico lo stato in draft
	  mvc.perform(get("/organizations/UPDANDPBH_12345678901/debtpositions/12345678901IUPDMOCK1")
	      .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
	  .andExpect(content().contentType(MediaType.APPLICATION_JSON))
	  .andExpect(MockMvcResultMatchers.jsonPath("$.status")
	      .value(DebtPositionStatus.DRAFT.toString()))
	  .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

	  // aggiorno e porto direttamente in pubblicata la posizione debitoria
	  mvc.perform(put("/organizations/UPDANDPBH_12345678901/debtpositions/12345678901IUPDMOCK1?toPublish=true")
	      .content(TestUtil.toJson(DebtPositionMock.getMock4())).contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isOk()).andExpect(content().contentType(MediaType.APPLICATION_JSON));

	  // verifico che lo stato sia stato settato a valid
	  mvc.perform(get("/organizations/UPDANDPBH_12345678901/debtpositions/12345678901IUPDMOCK1")
	      .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
	  .andExpect(content().contentType(MediaType.APPLICATION_JSON))
	  .andExpect(MockMvcResultMatchers.jsonPath("$.status")
	      .value(DebtPositionStatus.VALID.toString()))
	  .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

	  // provo a fare una nuova pubblicazione su una posizione debitoria con uno stato non più idoneo
	  mvc.perform(post("/organizations/UPDANDPBH_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
	      .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
	}
	
	@Test
	void updateDebtPosition_SegregationCode_403() throws Exception {
		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
		String iupd = paymentPositionDTO.getIupd();
		String iuv = paymentPositionDTO.getPaymentOption().get(0).getIuv();
		String notSufficientSegregationCode = "99";
		mvc.perform(post("/organizations/UPD_403_SC_12345678901/debtpositions")
							.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated())
				.andExpect(content().contentType(MediaType.APPLICATION_JSON))
				.andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
								   .value(auxDigit+iuv));

		mvc.perform(put("/organizations/UPD_403_SC_12345678901/debtpositions/" + iupd
								+ "?segregationCodes=" + notSufficientSegregationCode)
							.content(TestUtil.toJson(DebtPositionMock.getMock4()))
							.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isForbidden());
	}

	@Test
	void updateDebtPosition_400() throws Exception {
		// chiamata di aggiornamento della posizione debitoria con IUPD differenti
		mvc.perform(put("/organizations/400_12345678901/debtpositions/FAKE_12345678901IUPDMOCK1")
				.content(TestUtil.toJson(DebtPositionMock.getMock4()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest());
	}

	@Test
	void updateDebtPosition_No_Body_400() throws Exception {
		// chiamata di aggiornamento della posizione debitoria con IUPD differenti
		mvc.perform(put("/organizations/400_12345678901/debtpositions/FAKE_12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest());
	}

	@Test
	void updateDebtPosition_Invalid_Input_400() throws Exception {
		// creo una posizione debitoria 
		mvc.perform(post("/organizations/400_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// aggiorno la posizione debitoria mettendo una due_date < current date -> devo ricevere errore 400
		mvc.perform(put("/organizations/400_12345678901/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(DebtPositionMock.get400Mock3()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest());

	}

	@Test
	void updateDebtPosition_404() throws Exception {	
		// chiamata di aggiornamento della posizione debitoria con IUPD inesistente
		mvc.perform(put("/organizations/UPD_12345678901/debtpositions/12345678901IUPD400MOCK1")
				.content(TestUtil.toJson(DebtPositionMock.get400Mock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound());
	}

	@Test
	void updateDebtPosition_409() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/UPD409_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// recupero la posizione debitoria e verifico lo stato in draft
		mvc.perform(get("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.DRAFT.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// verifico che lo stato sia stato aggiornato a valid (doppio passaggio di stato) 
		mvc.perform(get("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.VALID.toString()))
		.andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

		// invalido la posizione debitoria
		mvc.perform(post("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1/invalidate")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.INVALID.toString()));

		// aggiorno la posizione debitoria con stato INVALID -> errore 409 (non deve essere possibile) 
		mvc.perform(put("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(DebtPositionMock.getMock4()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
	}
	
	@Test
	void updateDebtPosition_PAID_409() throws Exception {
		// creo una posizione debitoria (senza 'validity date' impostata)
		mvc.perform(post("/organizations/UPD409_PAID_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		// porto in pubblicata/validata lo stato della posizione debitoria
		mvc.perform(post("/organizations/UPD409_PAID_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// effettuo la notifica di pagamento e verifico lo stato in paid
		mvc.perform(post("/organizations/UPD409_PAID_12345678901/paymentoptions/"+auxDigit+"123456IUVMOCK1/pay")
				.content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(PaymentOptionStatus.PO_PAID.toString()));

		// recupero l'intera posizione debitoria e verifico lo stato in paid
		mvc.perform(get("/organizations/UPD409_PAID_12345678901/debtpositions/12345678901IUPDMOCK1")
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.status")
				.value(DebtPositionStatus.PAID.toString()));

		// provo ad aggiornare la posizione debitoria con stato già in PAID -> errore 409 (non deve essere possibile) 
		mvc.perform(put("/organizations/UPD409_PAID_12345678901/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(DebtPositionMock.getMock4()))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isConflict());
	}

	@Test
	void updateDebtPosition_noValidTransfer_422() throws Exception {

		PaymentPositionDTO paymentPositionDTO = DebtPositionMock.paymentPositionForNotificationUpdateMock1();
		
		when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class))).thenReturn(NodeCheckPositionResponse.builder().outcome("OK").build());

		// creo una posizione debitoria e recupero la payment option associata
		mvc.perform(post("/organizations/UPD422_novalidtransfer_12345678901/debtpositions")
						.content(TestUtil.toJson(paymentPositionDTO))
						.contentType(MediaType.APPLICATION_JSON))
				.andExpect(status().isCreated());

		// effettuo un aggiornamento della notification fee (si continua ad utilizzare lo IUV e non il NAV)
		mvc.perform(MockMvcRequestBuilders.put("/organizations/UPD422_novalidtransfer_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
				.content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(150L)))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

		// effettuo la chiamata di modifica del codice fiscale dell'EC del transfer ma non posso procedere perche eliminerei tutti i transfer associabili per la fee
		paymentPositionDTO.getPaymentOption().get(0).getTransfer().get(0).setOrganizationFiscalCode("acreditorinstitution");
		mvc.perform(MockMvcRequestBuilders.put("/organizations/UPD422_novalidtransfer_12345678901/debtpositions/12345678901IUPDMOCK1")
				.content(TestUtil.toJson(paymentPositionDTO))
				.contentType(MediaType.APPLICATION_JSON)).andExpect(status().isUnprocessableEntity());
	}
}
