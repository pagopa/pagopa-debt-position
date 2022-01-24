package it.gov.pagopa.debtposition.controller;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(SpringExtension.class)
@WebMvcTest
class BaseControllerTest {
  @Autowired
  private MockMvc mockMvc;

  @Test
  void shouldRespondOKtoHeartBeat() throws Exception {
    mockMvc.perform(get("/")).andExpect(status().isOk());
  }
}