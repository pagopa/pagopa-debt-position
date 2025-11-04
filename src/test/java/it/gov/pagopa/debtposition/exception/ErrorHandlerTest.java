package it.gov.pagopa.debtposition.exception;

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.http.HttpStatus;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.bind.annotation.*;

class ErrorHandlerTest {

  private MockMvc mvc;

  @RestController
  static class TestController {
	  
    // ========== ODP endpoints (match "/payment-options/") ==========

    @GetMapping("/payment-options/error")
    public String boom() {
      throw new RuntimeException("unexpected error");
    }

    @GetMapping("/payment-options/app-not-found")
    public String appNotFound() {
      throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, "70000000000", "NAV123");
    }

    @GetMapping("/payment-options/badparam/{id}")
    public String badParam(@PathVariable Integer id) {
      return "ok";
    }

    @GetMapping("/payment-options/missing")
    public String missing(@RequestParam String required) {
      return "ok";
    }

    // ========== non-ODP endpoint ==========
    @GetMapping("/other/app-not-found")
    public String otherNotFound() {
      throw new AppException(HttpStatus.NOT_FOUND, "Not found", "resource missing");
    }
  }
  
  @BeforeEach
  void setup() {
    this.mvc = MockMvcBuilders
        .standaloneSetup(new TestController())
        .setControllerAdvice(new ErrorHandler())
        .build();
  }


  // ---------------- ODP branch: generic exception => SYSTEM (ODP-103) 500 ----------------
  @Test
  void odp_genericException_returnsSystem500() throws Exception {
    mvc.perform(get("/payment-options/error").accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isInternalServerError())
        .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.httpStatusCode").value(500))
        .andExpect(jsonPath("$.appErrorCode").value("ODP-103"))
        .andExpect(jsonPath("$.errorMessage", containsString("PAA_SYSTEM_ERROR")));
  }

  // ---------------- ODP branch: AppException mapped to ODP ----------------
  @Test
  void odp_appException_mappedToOdp107() throws Exception {
    mvc.perform(get("/payment-options/app-not-found").accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.httpStatusCode").value(404))
        .andExpect(jsonPath("$.appErrorCode").value("ODP-107"))
        .andExpect(jsonPath("$.errorMessage", containsString("PAA_PAGAMENTO_SCONOSCIUTO")));
  }

  // ---------------- ODP branch: type mismatch => SYNTAX (ODP-101) 400 ----------------
  @Test
  void odp_typeMismatch_returnsSyntax400() throws Exception {
    mvc.perform(get("/payment-options/badparam/notANumber").accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.httpStatusCode").value(400))
        .andExpect(jsonPath("$.appErrorCode").value("ODP-101"))
        .andExpect(jsonPath("$.errorMessage", containsString("PAA_SINTASSI")));
  }

  // ---------------- ODP branch: missing request param => SYNTAX (ODP-101) 400 ----------------
  @Test
  void odp_missingRequestParam_returnsSyntax400() throws Exception {
    mvc.perform(get("/payment-options/missing").accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.httpStatusCode").value(400))
        .andExpect(jsonPath("$.appErrorCode").value("ODP-101"))
        .andExpect(jsonPath("$.errorMessage", containsString("PAA_SINTASSI")));
  }

  // ---------------- Legacy branch: AppException -> ProblemJson ----------------
  @Test
  void legacy_appException_returnsProblemJson() throws Exception {
    mvc.perform(get("/other/app-not-found").accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.status").value(404))
        .andExpect(jsonPath("$.title").value("Not found"))
        .andExpect(jsonPath("$.detail").value("resource missing"));
  }
}
