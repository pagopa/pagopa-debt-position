package it.gov.pagopa.debtposition.controller.pd.crud.api;

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
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionsInfo;
import it.gov.pagopa.debtposition.model.pd.response.PaymentPositionModelBaseResponse;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import java.time.LocalDate;


@Tag(name = "Debt Positions API")
@RequestMapping
public interface IDebtPositionController {

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
    ResponseEntity<PaymentPositionModel> createDebtPosition(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") String organizationFiscalCode,
            @Valid @RequestBody PaymentPositionModel paymentPositionModel,
            @RequestParam(required = false, defaultValue = "false") boolean toPublish);


    @Operation(summary = "Return the details of a specific debt position.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "getOrganizationDebtPositionByIUPD")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Obtained debt position details.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(name = "PaymentPositionResponse", implementation = PaymentPositionModelBaseResponse.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No debt position found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/organizations/{organizationfiscalcode}/debtpositions/{iupd}",
            produces = {"application/json"})
    ResponseEntity<PaymentPositionModelBaseResponse> getOrganizationDebtPositionByIUPD(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") String organizationFiscalCode,
            @Parameter(description = "IUPD (Unique identifier of the debt position). Format could be `<Organization fiscal code + UUID>` this would make it unique within the new PD management system. It's the responsibility of the EC to guarantee uniqueness. The pagoPa system shall verify that this is `true` and if not, notify the EC.", required = true)
            @PathVariable("iupd") String iupd);


    @Operation(summary = "Return the list of the organization debt positions. " +
                                 "The due dates interval is mutually exclusive with the payment dates interval.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "getOrganizationDebtPositions")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Obtained all organization payment positions.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = PaymentPositionsInfo.class))),
            @ApiResponse(responseCode = "400", description = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "429", description = "Too many requests.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/organizations/{organizationfiscalcode}/debtpositions",
            produces = {"application/json"})
    ResponseEntity<PaymentPositionsInfo> getOrganizationDebtPositions(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") String organizationFiscalCode,
            @Positive @Parameter(description = "Number of elements on one page. Default = 50") @RequestParam(required = false, defaultValue = "50") Integer limit,
            @Positive @Parameter(description = "Page number. Page value starts from 0", required = true) @RequestParam Integer page,
            @Valid @Parameter(description = "Filter from due_date (if provided use the format yyyy-MM-dd). If not provided will be set to 30 days before the due_date_to.") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam(value = "due_date_from", required = false) LocalDate dueDateFrom,
            @Valid @Parameter(description = "Filter to due_date (if provided use the format yyyy-MM-dd). If not provided will be set to 30 days after the due_date_from.") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam(value = "due_date_to", required = false) LocalDate dueDateTo,
            @Valid @Parameter(description = "Filter from payment_date (if provided use the format yyyy-MM-dd). If not provided will be set to 30 days before the payment_date_to.") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam(value = "payment_date_from", required = false)  LocalDate paymentDateFrom,
            @Valid @Parameter(description = "Filter to payment_date (if provided use the format yyyy-MM-dd). If not provided will be set to 30 days after the payment_date_from") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam(value = "payment_date_to", required = false)  LocalDate paymentDateTo,
            @Valid @Parameter(description = "Filter by debt position status") @RequestParam(value = "status", required = false) DebtPositionStatus status,
            @RequestParam(required = false, name = "orderby", defaultValue = "COMPANY_NAME") @Parameter(description = "Order by INSERTED_DATE, COMPANY_NAME, IUPD or STATUS") Order.PaymentPositionOrder orderBy,
            @RequestParam(required = false, name = "ordering", defaultValue = "DESC") @Parameter(description = "Direction of ordering") Sort.Direction ordering);


    @Operation(summary = "The Organization deletes a debt position", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "deletePosition")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Operation completed successfully."),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No debt position position found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "409", description = "Conflict: existing related payment found.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @DeleteMapping(value = "/organizations/{organizationfiscalcode}/debtpositions/{iupd}",
            produces = {"application/json"})
    ResponseEntity<String> deleteDebtPosition(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") String organizationFiscalCode,
            @Parameter(description = "IUPD (Unique identifier of the debt position). Format could be `<Organization fiscal code + UUID>` this would make it unique within the new PD management system. It's the responsibility of the EC to guarantee uniqueness. The pagoPa system shall verify that this is `true` and if not, notify the EC.", required = true)
            @PathVariable("iupd") String iupd);


    @Operation(summary = "The Organization updates a debt position ", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "updatePosition")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Request updated."),
            @ApiResponse(responseCode = "400", description = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No debt position found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "409", description = "Conflict: existing related payment found.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @PutMapping(value = "/organizations/{organizationfiscalcode}/debtpositions/{iupd}",
            produces = {"application/json"},
            consumes = {"application/json"})
    ResponseEntity<PaymentPositionModel> updateDebtPosition(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
            @PathVariable("organizationfiscalcode") String organizationFiscalCode,
            @Parameter(description = "IUPD (Unique identifier of the debt position). Format could be `<Organization fiscal code + UUID>` this would make it unique within the new PD management system. It's the responsibility of the EC to guarantee uniqueness. The pagoPa system shall verify that this is `true` and if not, notify the EC.", required = true)
            @PathVariable("iupd") String iupd,
            @Valid @RequestBody PaymentPositionModel paymentPositionModel);
}
