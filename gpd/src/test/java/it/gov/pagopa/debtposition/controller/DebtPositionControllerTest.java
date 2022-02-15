package it.gov.pagopa.debtposition.controller;

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
class DebtPositionControllerTest {

	@Autowired
	private MockMvc mvc;

	@Mock
	private ModelMapper modelMapperMock;

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
		.andExpect(status().isCreated());
	}

	@Test
	void createDebtPosition_Multiple_201() throws Exception {
		// creazione di due posizione debitorie per la stessa organizzazione
		mvc.perform(post("/organizations/12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(post("/organizations/12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());
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
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
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
		// creo due posizioni debitorie e le recupero
		mvc.perform(post("/organizations/LIST_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(post("/organizations/LIST_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		String url = "/organizations/LIST_12345678901/debtpositions?page=0";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				.value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
				.value(Matchers.hasSize(3)));
	}

	@Test
	void getDebtPositionListDueDateBetween() throws Exception {
		// creo due posizioni debitorie ed estraggo per intervallo di date
		mvc.perform(post("/organizations/DUEDATEBETWEEN_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(post("/organizations/DUEDATEBETWEEN_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/DUEDATEBETWEEN_12345678901/debtpositions?page=0" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC)) + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(1, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
				.value(Matchers.hasSize(3)));
	}

	@Test
	void getDebtPositionListDueDateGreaterThanOrEqual() throws Exception {
		// creo due posizioni debitorie ed estraggo quelle con due_date >= due_date_from
		mvc.perform(post("/organizations/DUEDATEGREATER_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(post("/organizations/DUEDATEGREATER_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/DUEDATEGREATER_12345678901/debtpositions?page=0" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(3, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				.value(Matchers.hasSize(1)));
	}

	@Test
	void getDebtPositionListDueDateLessThanOrEqual() throws Exception {
		// creo due posizioni debitorie ed estraggo quelle con due_date <= due_date_to
		mvc.perform(post("/organizations/DUEDATELESS_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock2())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		mvc.perform(post("/organizations/DUEDATELESS_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock3())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

		DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
		String url = "/organizations/DUEDATELESS_12345678901/debtpositions?page=0" + "&due_date_to="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(3, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
				.value(Matchers.hasSize(3)));
	}

	@Test
	void getDebtPositionList_404() throws Exception {
		// provo a recuperare una posizione debitoria che non esiste
		String url = "/organizations/LIST404_12345678901/debtpositions";
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
	}

	@Test
	void getDebtPositionListDueDate_404() throws Exception {
		// provo a recuperare una posizione debitoria passando una due_date_from con un formato diverso da quello atteso
		DateTimeFormatter df = DateTimeFormatter.ofPattern("dd-MM-yyyy");
		String url = "/organizations/LIST404_12345678901/debtpositions?page=0" + "&due_date_from="
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(3, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON));
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
		mvc.perform(post("/organizations/DEL_409_12345678901/paymentoptions/123456IUVMOCK1/pay")
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

	/**
	 * UPDATE DEBT POSITION
	 */
	@Test
	void updateDebtPosition_200() throws Exception {
		// creo una posizione debitoria 
		mvc.perform(post("/organizations/UPD_12345678901/debtpositions")
				.content(TestUtil.toJson(DebtPositionMock.getMock1())).contentType(MediaType.APPLICATION_JSON))
		.andExpect(status().isCreated());

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
				.value(1000));

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
				.value(500));

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
		mvc.perform(post("/organizations/UPD409_PAID_12345678901/paymentoptions/123456IUVMOCK1/pay")
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
}
