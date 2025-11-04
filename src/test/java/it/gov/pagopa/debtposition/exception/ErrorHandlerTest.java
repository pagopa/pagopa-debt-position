package it.gov.pagopa.debtposition.exception;

import static org.hamcrest.Matchers.containsString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import org.hibernate.exception.ConstraintViolationException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.aop.framework.AopConfigException;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.validation.annotation.Validated;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;
import org.springframework.validation.beanvalidation.MethodValidationInterceptor;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;

import jakarta.validation.Validator;

class ErrorHandlerTest {

	private MockMvc mvc;

	// simple dto for validation test
	static class SampleDto {
		@NotNull
		public String required;
	}

	@RestController
	@Validated
	static class TestController {

		// ========== ODP endpoint ==========
		@PostMapping(
			value = "/payment-options/organizations/{organizationFiscalCode}/notices/{noticeNumber}",
			consumes = MediaType.APPLICATION_JSON_VALUE,
			produces = MediaType.APPLICATION_JSON_VALUE)
		public String odpPost(
			@PathVariable String organizationFiscalCode,
			@PathVariable String noticeNumber,

			// used to trigger different branches in ErrorHandler
			@RequestParam(required = false) String mode,

			// MissingServletRequestParameterException on ODP branch when absent
			@RequestParam String requiredParam,

			// TypeMismatch on ODP branch when non-numeric
			@RequestParam(required = false) Integer size,

			// ConstraintViolation on ODP branch when < 1
			@RequestParam(required = false) @Min(1) Integer minParam,

			/**
			 * Triggers MethodArgumentNotValid when body == {}.
			 */
			@Valid @RequestBody(required = false) SampleDto dto) {

			if ("error".equals(mode)) {
				throw new RuntimeException("unexpected error");
			}
			if ("app-not-found".equals(mode)) {
				throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, "70000000000", "NAV123");
			}
			if ("app-not-payable".equals(mode)) {
				// ODP-110 (422, PAA_PAGAMENTO_SCADUTO)
				throw new AppException(AppError.PAYMENT_OPTION_NOT_PAYABLE, "70000000000", "NAV999");
			}
			if ("app-unmapped-system".equals(mode)) {
			    // Force ODP generic SYSTEM fallback (ODP-103 / 500)
			    throw new AppException(HttpStatus.INTERNAL_SERVER_ERROR, "Internal error", "unexpected exception");
			}
			if ("data-integrity-any".equals(mode)) {
				// DataIntegrity violation -> ODP SYSTEM (500)
				throw new DataIntegrityViolationException("generic DI violation");
			}

			return "ok";
		}

		// ========== non-ODP endpoint ==========
		@GetMapping("/other/app-not-found")
		public String otherNotFound() {
			throw new AppException(HttpStatus.NOT_FOUND, "Not found", "resource missing");
		}

		@GetMapping("/other/badparam/{id}")
		public String otherBadParam(@PathVariable Integer id) {
			return "ok";
		}

		@PostMapping(value = "/other/json", consumes = MediaType.APPLICATION_JSON_VALUE)
		public String otherNeedsJson(@Valid @RequestBody SampleDto dto) {
			return "ok";
		}

		@GetMapping("/other/data-integrity-fk")
		public String otherDataIntegrityFk() {
			ConstraintViolationException h = mock(ConstraintViolationException.class);
			when(h.getSQLState()).thenReturn(ErrorHandler.FOREIGN_KEY_VIOLATION);
			throw new DataIntegrityViolationException("FK", h);
		}

		@GetMapping("/other/data-integrity-generic")
		public String otherDataIntegrityGeneric() {
			throw new DataIntegrityViolationException("generic DI violation");
		}
	}

	@BeforeEach
	void setup() {
		TestController target = new TestController();
		try (
			// Jakarta Validator
			LocalValidatorFactoryBean validatorFactory = new LocalValidatorFactoryBean()) {
				validatorFactory.afterPropertiesSet();
				Validator jakartaValidator = validatorFactory.getValidator();

				MethodValidationInterceptor interceptor = new MethodValidationInterceptor(jakartaValidator);

				// add validation to proxy
				ProxyFactory pf = new ProxyFactory(target);
				pf.addAdvice(interceptor);
				Object proxiedController = pf.getProxy();

				this.mvc = MockMvcBuilders
						.standaloneSetup(proxiedController)
						.setControllerAdvice(new ErrorHandler())
						.build();
		} catch (AopConfigException e) {
			e.printStackTrace();
		}
	}


	// ---------------- ODP branch: generic exception => SYSTEM (ODP-103) 500 ----------------
	@Test
	void odp_genericException_returnsSystem500() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				.param("mode", "error")
				.param("requiredParam", "ok")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON))
		.andExpect(status().isInternalServerError())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(500))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-103"))
		.andExpect(jsonPath("$.errorMessage", containsString("PAA_SYSTEM_ERROR")));
	}

	//---------------- ODP branch: AppException not in overlay -> fallback SYSTEM (ODP-103) 500 ----------------
	@Test
	void odp_appException_unmapped_fallsBackToSystem500() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				.param("mode", "app-unmapped-system")
				.param("requiredParam", "ok")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON))
		.andExpect(status().isInternalServerError())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(500))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-103"))
		.andExpect(jsonPath("$.errorMessage", containsString("PAA_SYSTEM_ERROR")));
	}

	//---------------- ODP branch: DataIntegrityViolationException => SYSTEM (ODP-103) 500 ----------------
	@Test
	void odp_dataIntegrity_any_returnsSystem500() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				.param("mode", "data-integrity-any")
				.param("requiredParam", "ok")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON))
		.andExpect(status().isInternalServerError())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(500))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-103"));
	}

	// ---------------- ODP branch: AppException mapped to ODP ----------------
	@Test
	void odp_appException_mappedToOdp107() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				.param("mode", "app-not-found")
				.param("requiredParam", "ok")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON))
		.andExpect(status().isNotFound())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(404))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-107"))
		.andExpect(jsonPath("$.errorMessage", containsString("PAA_PAGAMENTO_SCONOSCIUTO")));
	}

	//---------------- ODP branch: AppException with override 422 -> ODP-110 ----------------
	@Test
	void odp_appException_mappedToOdp110_422() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				.param("mode", "app-not-payable")
				.param("requiredParam", "ok")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON))
		.andExpect(status().isUnprocessableEntity())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(422))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-110"))
		.andExpect(jsonPath("$.errorMessage", containsString("PAA_PAGAMENTO_SCADUTO")));
	}

	// ---------------- ODP branch: type mismatch => SYNTAX (ODP-101) 400 ----------------
	@Test
	void odp_typeMismatch_returnsSyntax400() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				.param("requiredParam", "ok")
				.param("size", "notANumber")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON)
				.content("{}"))
		.andExpect(status().isBadRequest())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(400))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-101"))
		.andExpect(jsonPath("$.errorMessage", containsString("PAA_SINTASSI")));
	}

	// ---------------- ODP branch: missing request param => SYNTAX (ODP-101) 400 ----------------
	@Test
	void odp_missingRequestParam_returnsSyntax400() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				// requiredParam intentionally missing
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON)
				.content("{}"))
		.andExpect(status().isBadRequest())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(400))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-101"))
		.andExpect(jsonPath("$.errorMessage", containsString("PAA_SINTASSI")));
	}

	//---------------- ODP branch: constraint violation on request param => SYNTAX (ODP-101) 400 ----------------
	@Test
	void odp_paramConstraintViolation_returnsSyntax400() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				.param("requiredParam", "ok")
				.param("minParam", "0") // violates @Min(1)
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON)
				.content("{}"))
		.andExpect(status().isBadRequest())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(400))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-101"))
		.andExpect(jsonPath("$.errorMessage", containsString("PAA_SINTASSI")));
	}

	//---------------- ODP branch: unreadable JSON body => SYNTAX (ODP-101) 400 ----------------
	@Test
	void odp_httpMessageNotReadable_returnsSyntax400() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				.param("requiredParam", "ok")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON)
				.content("{invalidJson"))
		.andExpect(status().isBadRequest())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(400))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-101"));
	}

	//---------------- ODP branch: bean validation @Valid => SYNTAX (ODP-101) 400 ----------------
	@Test
	void odp_methodArgumentNotValid_returnsSyntax400() throws Exception {
		mvc.perform(post("/payment-options/organizations/70000000000/notices/123456")
				.param("requiredParam", "ok")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON)
				.content("{}")) // missing required field in body
		.andExpect(status().isBadRequest())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.httpStatusCode").value(400))
		.andExpect(jsonPath("$.appErrorCode").value("ODP-101"));
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

	//---------------- Legacy branch: type mismatch -> ProblemJson 400 ----------------
	@Test
	void legacy_typeMismatch_returnsProblemJson400() throws Exception {
		mvc.perform(get("/other/badparam/notANumber").accept(MediaType.APPLICATION_JSON))
		.andExpect(status().isBadRequest())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.status").value(400))
		.andExpect(jsonPath("$.title").value("BAD REQUEST"));
	}

	// ---------------- Legacy branch: unreadable JSON -> ProblemJson 400 ----------------
	@Test
	void legacy_httpMessageNotReadable_returnsProblemJson400() throws Exception {
		mvc.perform(post("/other/json")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON)
				.content("{invalidJson"))
		.andExpect(status().isBadRequest())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.status").value(400))
		.andExpect(jsonPath("$.title").value("BAD REQUEST"));
	}

	// ---------------- Legacy branch: method argument not valid -> ProblemJson 400 ----------------
	@Test
	void legacy_methodArgumentNotValid_returnsProblemJson400() throws Exception {
		mvc.perform(post("/other/json")
				.contentType(MediaType.APPLICATION_JSON)
				.accept(MediaType.APPLICATION_JSON)
				.content("{}"))
		.andExpect(status().isBadRequest())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.status").value(400))
		.andExpect(jsonPath("$.title").value("BAD REQUEST"));
	}

	// ---------------- Legacy branch: DataIntegrityViolation with FK -> 409 ProblemJson ----------------
	@Test
	void legacy_dataIntegrity_fk_returns409ProblemJson() throws Exception {
		mvc.perform(get("/other/data-integrity-fk").accept(MediaType.APPLICATION_JSON))
		.andExpect(status().isConflict())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.status").value(409))
		.andExpect(jsonPath("$.title", containsString("Conflict")));
	}

	// ---------------- Legacy branch: DataIntegrityViolation generic -> 500 ProblemJson ----------------
	@Test
	void legacy_dataIntegrity_generic_returns500ProblemJson() throws Exception {
		mvc.perform(get("/other/data-integrity-generic").accept(MediaType.APPLICATION_JSON))
		.andExpect(status().isInternalServerError())
		.andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
		.andExpect(jsonPath("$.status").value(500))
		.andExpect(jsonPath("$.title").value("INTERNAL SERVER ERROR"));
	}
}
