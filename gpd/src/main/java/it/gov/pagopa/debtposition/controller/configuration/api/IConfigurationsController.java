package it.gov.pagopa.debtposition.controller.configuration.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import it.gov.pagopa.debtposition.model.ProblemJson;
import it.gov.pagopa.debtposition.model.payments.response.OrganizationListModelResponse;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.validation.Valid;
import java.time.LocalDate;


@Tag(name = "Configurations API")
@RequestMapping
public interface IConfigurationsController {

    @Operation(summary = "Return the list of the organizations.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "getOrganizations", tags = {"Get Organization List"})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Obtained organizations to add and delete.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = OrganizationListModelResponse.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/organizations",
            produces = {"application/json"})
	ResponseEntity<OrganizationListModelResponse> getOrganizations(
			@Valid @Parameter(description = "Filter from date (use the format yyyy-MM-dd)") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) @RequestParam(value = "since", required = true)
					LocalDate since);

    @Operation(summary = "Check the existence of the organization.", security = {@SecurityRequirement(name = "ApiKey"), @SecurityRequirement(name = "Authorization")}, operationId = "checkOrganization", tags = {"Check Organization"})
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "The organization is registered.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = String.class))),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No organization found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class)))})
    @GetMapping(value = "/organizations/{organizationfiscalcode}",
            produces = {"application/json"})
	ResponseEntity<String> checkOrganization(
			@Parameter(description = "Organization fiscal code, the fiscal code of the Organization.", required = true)
			@PathVariable("organizationfiscalcode") String organizationFiscalCode);

}
