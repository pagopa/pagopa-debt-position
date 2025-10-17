package it.gov.pagopa.debtposition.controller.concurrent;

import static org.hamcrest.Matchers.oneOf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.mock.DebtPositionMock;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@SpringBootTest
@AutoConfigureMockMvc
// TODO: fix these tests with CONCURRENT mode enabled
// @Execution(ExecutionMode.CONCURRENT)
@TestInstance(Lifecycle.PER_CLASS)
@Slf4j
class ConcurrentPartiallyReportedPaymentsControllerTest {

  @Autowired private MockMvc mvc;

  @Value("${nav.aux.digit}")
  private String auxDigit = "3";

  @BeforeAll
  void initDebtPosition_for_partially_reporting() throws Exception {
    log.trace(
        "initDebtPosition_for_partially_reporting start => " + Thread.currentThread().getName());
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/1234567890100000/debtpositions?toPublish=true")
                .content(TestUtil.toJson(DebtPositionMock.getMock8()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // effettuo la notifica di pagamento
    mvc.perform(
            post("/organizations/1234567890100000/paymentoptions/"
                    + auxDigit
                    + "1234568/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
    mvc.perform(
            get("/organizations/1234567890100000/debtpositions/12345678901IUPDMULTIPLEMOCK8")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));
    log.trace(
        "initDebtPosition_for_partially_reporting end => " + Thread.currentThread().getName());
  }

  // Concurrency testing: error "Batch update returned unexpected row count from update [0]; actual
  // row count: 0; expected: 1;" (see task PAGOPA-1616)
  @ParameterizedTest
  @ValueSource(ints = {1, 2, 3, 4, 5})
  void concurrent_partiallyReportedTransfer_thread(int number) throws Exception {
    log.trace(
        "concurrent_partiallyReportedTransfer_thread for transfers with id 1 - thread("
            + number
            + ") start => "
            + Thread.currentThread().getName());
    // effettuo la rendicontazione per una delle 2 transazioni della PO (si continua ad utilizzare
    // lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/1234567890100000/paymentoptions/1234568/transfers/1/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().is(oneOf(200, 409)));
    log.trace(
        "concurrent_partiallyReportedTransfer_thread for transfers with id 1 - thread("
            + number
            + ") end => "
            + Thread.currentThread().getName());
  }

  @AfterAll
  void checkDebtPosition_after_partially_reporting() throws Exception {
    log.trace(
        "checkDebtPosition_after_partially_reporting start => " + Thread.currentThread().getName());
    // recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
    String url =
        "/organizations/1234567890100000/paymentoptions/"
            + auxDigit
            + "1234568";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PARTIALLY_REPORTED.toString()));

    // recupero la PP e verifico lo stato sia rimasto PAID
    mvc.perform(
            get("/organizations/1234567890100000/debtpositions/12345678901IUPDMULTIPLEMOCK8")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));
    log.trace(
        "checkDebtPosition_after_partially_reporting end => " + Thread.currentThread().getName());
  }
}
