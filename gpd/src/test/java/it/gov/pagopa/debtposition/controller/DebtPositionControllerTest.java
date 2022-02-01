package it.gov.pagopa.debtposition.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
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
import it.gov.pagopa.debtposition.service.DebtPositionService;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class DebtPositionControllerTest {

    @Autowired
    private MockMvc mvc;

    @Mock
    private ModelMapper modelMapperMock;

    @Mock
    private DebtPositionService debtPositionService;

    @BeforeEach
    void setUp() {
    }

    // CREATE DEBT POSITION
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

    // GET DEBT POSITION BY IUPD
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

    // GET LIST DEBT POSITIONS
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
                        .value(Matchers.hasSize(2)));
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
                        .value(Matchers.hasSize(2)));
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
                        .value(Matchers.hasSize(2)));
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

}
