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
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
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
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				.value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
				.value(Matchers.hasSize(3)));
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
							 + df.format(LocalDateTime.now(ZoneOffset.UTC));
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
		mvc.perform(post("/organizations/LIST_12345678902/paymentoptions/123456IUVMOCK1/pay")
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
		mvc.perform(post("/organizations/LIST_12345678903/paymentoptions/123456IUVMOCK1/pay")
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
				+ df.format(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.DAYS));
		mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
		.andExpect(content().contentType(MediaType.APPLICATION_JSON))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd").value(Matchers.hasSize(2)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
				// manca la payment_option che ha una due_date maggiore di quella inserita nella ricerca
				.value(Matchers.hasSize(1)))
		.andExpect(MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
				.value(Matchers.hasSize(3)));
	}
/*
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
*/
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
