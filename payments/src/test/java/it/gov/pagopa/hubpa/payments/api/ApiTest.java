package it.gov.pagopa.hubpa.payments.api;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import it.gov.pagopa.hubpa.payments.service.PartnerService;
import it.gov.pagopa.hubpa.payments.service.PaymentService;

@ExtendWith(SpringExtension.class)
@WebMvcTest
class ApiTest {
  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private PaymentService paymentService;

  @MockBean
  private PartnerService partnerService;

  @Test
  void shouldRespondOKtoHeartBeat() throws Exception {
    mockMvc.perform(get("/")).andExpect(status().isOk());
  }
}