package it.gov.pagopa.debtposition.controller.odp;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import it.gov.pagopa.debtposition.model.ProblemJson;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.odp.PaymentOptionModelOdp;
import it.gov.pagopa.debtposition.model.odp.PaymentPositionModelOdp;
import it.gov.pagopa.debtposition.model.odp.PaymentPositionsInfoOdp;
import it.gov.pagopa.debtposition.model.odp.response.PaymentPositionModelResponseOdp;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Positive;
import java.time.LocalDate;

@Tag(name = "Debt Positions API: Installments and Payment Options Manager")
@Validated
@RequestMapping("/options")
public interface IDebtPositionControllerOdp {

    /**
     * CREATE a debt position with the payment options composed by installment
     *
     * @param paymentPositionModel The PaymentPosition following OdP model
     */
    @Operation(summary = "The Organization creates a debt Position.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "createPosition")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Request created."),
            @ApiResponse(responseCode = "400", description = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "409", description = "Conflict: duplicate debt position found.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PostMapping(value = "/organizations/{organizationfiscalcode}/debtpositions",
            produces = MediaType.APPLICATION_JSON_VALUE,
            consumes = MediaType.APPLICATION_JSON_VALUE)
    ResponseEntity<PaymentPositionModelOdp> createDebtPosition(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") String organizationFiscalCode,
            @Valid @RequestBody PaymentPositionModelOdp paymentPositionModel,
            @RequestParam(required = false, defaultValue = "false") boolean toPublish,
            @Valid @Parameter(description = "Segregation codes for which broker is authorized", hidden = true) @Pattern(regexp = "\\d{2}(,\\d{2})*")
            @RequestParam(required = false) String segregationCodes,
            @Parameter(hidden = false, description="The field must not be considered as its value is set via the API Management (APIM) policy") @RequestParam(required = false, defaultValue = "GPD") ServiceType serviceType);


    /**
     * RETRIEVE a debt position with the payment options composed by installment
     *
     */
    @Operation(summary = "Return the list of the organization debt positions. " +
            "The due dates interval is mutually exclusive with the payment dates interval.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "getOrganizationDebtPositions")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Obtained all organization payment positions.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PaymentPositionsInfoOdp.class))),
            @ApiResponse(responseCode = "400", description = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/organizations/{organizationfiscalcode}/debtpositions",
            produces = {"application/json"})
    ResponseEntity<PaymentPositionsInfoOdp> getOrganizationDebtPositions(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") String organizationFiscalCode,
            @Valid @Positive @Max(50) @Parameter(description = "Number of elements on one page. Default = 50") @RequestParam(required = false, defaultValue = "10") Integer limit,
            @Valid @Min(0) @Parameter(description = "Page number. Page value starts from 0") @RequestParam(required = false, defaultValue = "0") Integer page,
            @Valid @Parameter(description = "Filter from due_date (if provided use the format yyyy-MM-dd). If not provided will be set to 30 days before the due_date_to.") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam(value = "due_date_from", required = false) LocalDate dueDateFrom,
            @Valid @Parameter(description = "Filter to due_date (if provided use the format yyyy-MM-dd). If not provided will be set to 30 days after the due_date_from.") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam(value = "due_date_to", required = false) LocalDate dueDateTo,
            @Valid @Parameter(description = "Filter from payment_date (if provided use the format yyyy-MM-dd). If not provided will be set to 30 days before the payment_date_to.") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam(value = "payment_date_from", required = false)  LocalDate paymentDateFrom,
            @Valid @Parameter(description = "Filter to payment_date (if provided use the format yyyy-MM-dd). If not provided will be set to 30 days after the payment_date_from") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam(value = "payment_date_to", required = false)  LocalDate paymentDateTo,
            @Valid @Parameter(description = "Filter by debt position status") @RequestParam(value = "status", required = false) DebtPositionStatus status,
            @RequestParam(required = false, name = "orderby", defaultValue = "INSERTED_DATE") @Parameter(description = "Order by INSERTED_DATE, COMPANY_NAME, IUPD or STATUS") Order.PaymentPositionOrder orderBy,
            @RequestParam(required = false, name = "ordering", defaultValue = "DESC") @Parameter(description = "Direction of ordering") Sort.Direction ordering,
            @Valid @Parameter(description = "Segregation codes for which broker is authorized", hidden = true) @Pattern(regexp = "\\d{2}(,\\d{2})*")
            @RequestParam(required = false) String segregationCodes);

    @Operation(summary = "Return the details of a specific debt position.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "getOrganizationDebtPositionByIUPD")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Obtained debt position details.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(name = "PaymentPositionResponse", implementation = PaymentPositionModelResponseOdp.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No debt position found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/organizations/{organizationfiscalcode}/debtpositions/{iupd}",
            produces = {"application/json"})
    ResponseEntity<PaymentPositionModelResponseOdp> getOrganizationDebtPositionByIUPD(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @Pattern(regexp = "[\\w*\\h-]+") @PathVariable("organizationfiscalcode") String organizationFiscalCode,
            @Parameter(description = "IUPD (Unique identifier of the debt position). Format could be `<Organization fiscal code + UUID>` this would make it unique within the new PD management system. It's the responsibility of the EC to guarantee uniqueness. The pagoPa system shall verify that this is `true` and if not, notify the EC.", required = true)
            @Pattern(regexp = "[\\w*\\h-]+") @PathVariable("iupd") String iupd,
            @Valid @Parameter(description = "Segregation codes for which broker is authorized", hidden = true) @Pattern(regexp = "\\d{2}(,\\d{2})*")
            @RequestParam(required = false) String segregationCodes);


    /**
     * UPDATE a debt position with the payment options composed by installment
     *
     * @param paymentPositionModel The PaymentPosition following OdP model
     */
    @Operation(summary = "The Organization updates a debt position ", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "updatePosition")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Debt Position updated."),
            @ApiResponse(responseCode = "400", description = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No debt position found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "409", description = "Conflict: existing related payment found.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PutMapping(value = "/organizations/{organizationfiscalcode}/debtpositions/{iupd}",
            produces = {"application/json"},
            consumes = {"application/json"})
    ResponseEntity<PaymentPositionModelOdp> updateDebtPosition(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") String organizationFiscalCode,
            @Parameter(description = "IUPD (Unique identifier of the debt position). Format could be `<Organization fiscal code + UUID>` this would make it unique within the new PD management system. It's the responsibility of the EC to guarantee uniqueness. The pagoPa system shall verify that this is `true` and if not, notify the EC.", required = true)
            @PathVariable("iupd") String iupd,
            @Valid @RequestBody PaymentPositionModelOdp paymentPositionModel,
            @RequestParam(required = false, defaultValue = "false") boolean toPublish,
            @Valid @Parameter(description = "Segregation codes for which broker is authorized", hidden = true) @Pattern(regexp = "\\d{2}(,\\d{2})*")
            @RequestParam(required = false) String segregationCodes);
}
