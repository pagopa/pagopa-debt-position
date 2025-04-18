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
class ConcurrentPartiallyPaidPaymentsControllerTest {

  @Autowired private MockMvc mvc;

  @Value("${nav.aux.digit}")
  private String auxDigit = "3";

  @BeforeAll
  void initDebtPosition_for_partially_paid() throws Exception {
    log.trace("initDebtPosition_for_partially_paid start => " + Thread.currentThread().getName());
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/PAY_Concurrent_Partially_Paid_12345678901/debtpositions?toPublish=true")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());
    log.trace("initDebtPosition_for_partially_paid end => " + Thread.currentThread().getName());
  }

  // Concurrency testing: error "Batch update returned unexpected row count from update [0]; actual
  // row count: 0; expected: 1;" (see task PAGOPA-1616)
  @ParameterizedTest
  @ValueSource(ints = {1, 2, 3, 4, 5})
  void concurrent_partiallyPaid_thread(int number) throws Exception {
    log.trace(
        "concurrent_partiallyPaid_thread - thread("
            + number
            + ") start => "
            + Thread.currentThread().getName());
    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/PAY_Concurrent_Partially_Paid_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK4/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().is(oneOf(200, 409)))
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    log.trace(
        "concurrent_partiallyPaid_thread - thread("
            + number
            + ") end => "
            + Thread.currentThread().getName());
  }

  @AfterAll
  void checkDebtPosition_after_partially_paid() throws Exception {
    log.trace(
        "checkDebtPosition_after_partially_paid start => " + Thread.currentThread().getName());
    // recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
    String url =
        "/organizations/PAY_Concurrent_Partially_Paid_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK4";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero la PP e verifico lo stato sia PARTIALLY_PAID
    mvc.perform(
            get("/organizations/PAY_Concurrent_Partially_Paid_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));
    log.trace("checkDebtPosition_after_partially_paid end => " + Thread.currentThread().getName());
  }
}
