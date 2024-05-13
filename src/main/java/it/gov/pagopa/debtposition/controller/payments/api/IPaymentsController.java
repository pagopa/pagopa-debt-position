package it.gov.pagopa.debtposition.controller.payments.api;

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
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.TransferModelResponse;
import it.gov.pagopa.debtposition.model.pd.NotificationFeeUpdateModel;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.Pattern;


@Tag(name = "Payments API")
@RequestMapping
public interface IPaymentsController {

    @Operation(summary = "Return the details of a specific payment option.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "getOrganizationPaymentOptionByNAV")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Obtained payment option details.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(name = "PaymentPositionResponse", implementation = PaymentOptionWithDebtorInfoModelResponse.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No payment option found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{nav}",
            produces = {"application/json"})
    ResponseEntity<PaymentOptionWithDebtorInfoModelResponse> getOrganizationPaymentOptionByNAV(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") @Pattern(regexp = "\\b\\w{11}\\b") String organizationFiscalCode,
            @Parameter(description = "NAV (notice number) is the unique reference assigned to the payment by a creditor institution.", required = true)
            @PathVariable("nav") String nav);

    @Operation(summary = "The Organization paid a payment option.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "payPaymentOption")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Request paid."),
            @ApiResponse(responseCode = "400", description = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No payment option found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "409", description = "Conflict: existing related payment found.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "422", description = "Unprocessable: not in payable state.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{nav}/pay",
            produces = {"application/json"},
            consumes = {"application/json"})
    ResponseEntity<PaymentOptionModelResponse> payPaymentOption(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") @Pattern(regexp = "\\b\\w{11}\\b") String organizationFiscalCode,
            @Parameter(description = "NAV (notice number) is the unique reference assigned to the payment by a creditor institution.", required = true)
            @PathVariable("nav") String nav,
            @Valid @RequestBody PaymentOptionModel paymentOptionModel);

    @Operation(summary = "The organization reports a transaction.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "reportTransfer")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Request reported."),
            @ApiResponse(responseCode = "400", description = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No transfer found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "409", description = "Conflict: existing related payment found.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{iuv}/transfers/{transferid}/report",
            produces = {"application/json"})
    ResponseEntity<TransferModelResponse> reportTransfer(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") @Pattern(regexp = "\\b\\w{11}\\b") String organizationFiscalCode,
            @Parameter(description = "IUV (Unique Payment Identification). Alphanumeric code that uniquely associates and identifies three key elements of a payment: reason, payer, amount", required = true)
            @PathVariable("iuv") String iuv,
            @Parameter(description = "Transaction identifier. Alphanumeric code that identifies the specific transaction", required = true)
            @PathVariable("transferid") String transferId);

    @Operation(summary = "The organization updates the notification fee of a payment option.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "updateNotificationFee")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Request updated."),
            @ApiResponse(responseCode = "209", description = "Request updated with a payment in progress."),
            @ApiResponse(responseCode = "400", description = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No payment option found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "422", description = "Unprocessable payment option.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PutMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{nav}/notificationfee",
            produces = {"application/json"})
    ResponseEntity<PaymentOptionModelResponse> updateNotificationFee(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") @Pattern(regexp = "\\b\\w{11}\\b") String organizationFiscalCode,
            @Parameter(description = "NAV (notice number) is the unique reference assigned to the payment by a creditor institution.", required = true)
            @PathVariable("nav") String nav,
            @Valid @RequestBody NotificationFeeUpdateModel notificationFeeUpdateModel);

}
