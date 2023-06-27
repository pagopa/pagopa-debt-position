package it.gov.pagopa.debtposition.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

import it.gov.pagopa.debtposition.DebtPositionApplication;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class BaseControllerTest {
  @Autowired
  private MockMvc mockMvc;

  @Test
  void shouldRespondOKtoHeartBeat() throws Exception {
    mockMvc.perform(get("/info")).andExpect(status().isOk());
  }
}