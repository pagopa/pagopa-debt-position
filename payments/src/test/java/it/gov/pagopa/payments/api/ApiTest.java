package it.gov.pagopa.payments.api;


import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;

import it.gov.pagopa.payments.PaymentsApplication;

@SpringBootTest(classes = PaymentsApplication.class)
@ExtendWith(SpringExtension.class)
@AutoConfigureMockMvc
class ApiTest {

    @Autowired
    private MockMvc mockMvc;


    @Test
    void shouldRespondOKtoHeartBeat() throws Exception {
        mockMvc.perform(get("/info")).andExpect(status().isOk());
    }

    @Test
    void applicationContextLoaded() {
        assertTrue(true); // it just tests that an error has not occurred
    }

    @Test
    void applicationContextTest() {
        PaymentsApplication.main(new String[]{});
        assertTrue(true); // it just tests that an error has not occurred
    }
}
