package it.gov.pagopa.debtposition.controller.pd.api;

import java.util.List;

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
import io.swagger.v3.oas.annotations.tags.Tag;
import it.gov.pagopa.debtposition.dto.DebtorDTO;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.model.ProblemJson;



@Tag(name = "Debtor Positions API")
@RequestMapping 
public interface IDebtPositionController {

    @Operation(summary = "The Organization creates a Debt Position ", operationId = "createPosition", tags={"Create Debt Positions"})
    @ApiResponses(value = { 
            @ApiResponse(responseCode  = "201", description  = "Request created."),
            @ApiResponse(responseCode  = "400", description  = "Malformed request.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode  = "401", description  = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode  = "409", description  = "Conflict: duplicate request found.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode  = "500", description  = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))) })
    @PostMapping(value = "/organizations/{organizationfiscalcode}/debtpositions",
    produces = { "application/json" }, 
    consumes = { "application/json" })
    ResponseEntity<String> createDebtPosition(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.",required=true) 
            @PathVariable("organizationfiscalcode") String organizationFiscalCode, 
            @Valid @RequestBody DebtorDTO debtPosition);



    @Operation(summary = "Return the details of a specific debt position. ", operationId = "getPositionByNoticeNumber", tags={"Get Debt Positions"})
    @ApiResponses(value = { 
            @ApiResponse(responseCode = "200", description = "Obtained payment position details.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
            @ApiResponse(responseCode = "401", description = "Wrong or missing function key.", content = @Content(schema = @Schema())),
            @ApiResponse(responseCode = "404", description = "No payment found.", content = @Content(schema = @Schema(implementation = ProblemJson.class))),
            @ApiResponse(responseCode = "500", description = "Service unavailable.", content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ProblemJson.class))) })
    @GetMapping(value = "/organizations/{organizationfiscalcode}/debtpositions/{iupd}",
    produces = { "application/json" })
    ResponseEntity<List<PaymentPositionDTO>> getDebtPositionByIUPD(
            @Parameter(description = "Organization fiscal code, the fiscal code of the Organization.",required=true) 
            @PathVariable("organizationfiscalcode") String organizationfiscalcode, 
            @Parameter(description = "IUPD (Unique identifier of the debt position). Format could be `<Organization fiscal code + UUID>` this would make it unique within the new PD management system. It's the responsibility of the EC to guarantee uniqueness. The pagoPa system shall verify that this is `true` and if not, notify the EC.",required=true) 
            @PathVariable("iupd") String iupd);


}
