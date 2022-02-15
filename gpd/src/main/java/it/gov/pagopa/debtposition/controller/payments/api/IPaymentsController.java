package it.gov.pagopa.debtposition.controller.payments.api;

import javax.validation.Valid;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import it.gov.pagopa.debtposition.model.ProblemJson;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;



@Tag(name = "Payments API")
@RequestMapping 
public interface IPaymentsController {
	
	@Operation(summary = "Return the details of a specific payment option.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "getOrganizationPaymentOptionByIUV", tags={"Get Payment Option"})
	@ApiResponses(value = { 
			@ApiResponse(responseCode = "200", description = "Obtained payment option details.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema= @Schema(name="PaymentPositionResponse", implementation = PaymentOptionModelResponse.class))),
			@ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
			@ApiResponse(responseCode = "404", description = "No payment option found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
			@ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))) })
	@GetMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{iuv}",
	produces = { "application/json" })
	ResponseEntity<PaymentOptionModelResponse> getOrganizationPaymentOptionByIUV(
			@Parameter(description = "Organization fiscal code, the fiscal code of the Organization.",required=true) 
			@PathVariable("organizationfiscalcode") String organizationFiscalCode, 
			@Parameter(description = "IUV (Unique Payment Identification). Alphanumeric code that uniquely associates and identifies three key elements of a payment: reason, payer, amount",required=true) 
			@PathVariable("iuv") String iuv	);
	
	@Operation(summary = "The Organization paid a payment option ", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "payPaymentOption", tags={ "Pay Debt Positions"})
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Request paid."),
        @ApiResponse(responseCode = "400", description = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
        @ApiResponse(responseCode = "401", description = "Wrong or missing function key."),
        @ApiResponse(responseCode = "404", description = "No debt position found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
        @ApiResponse(responseCode = "409", description = "Conflict: existing related payment found.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
        @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))) })
    @PostMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{iuv}/pay",
        produces = { "application/json" }, 
        consumes = { "application/json" })
    ResponseEntity<PaymentOptionModelResponse> payPaymentOption(
    		@Parameter(description = "Organization fiscal code, the fiscal code of the Organization.",required=true) 
    		@PathVariable("organizationfiscalcode") String organizationFiscalCode,
    		@Parameter(description = "IUV (Unique Payment Identification). Alphanumeric code that uniquely associates and identifies three key elements of a payment: reason, payer, amount",required=true) 
    		@PathVariable("iuv") String iuv, 
    		@Valid @RequestBody PaymentOptionModel paymentOptionModel);

}
