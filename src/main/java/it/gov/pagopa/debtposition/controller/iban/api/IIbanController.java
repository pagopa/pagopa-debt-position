package it.gov.pagopa.debtposition.controller.iban.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import it.gov.pagopa.debtposition.model.ProblemJson;
import it.gov.pagopa.debtposition.model.iban.UpdateIbanMassiveModel;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.validation.Valid;

@Tag(name = "Iban API")
@RequestMapping
public interface IIbanController {

    @Operation(
            summary = "The Organization updates the IBANs of every unpaid payment option & its transfers",
            security = {
                    @SecurityRequirement(name = "ApiKey"),
                    @SecurityRequirement(name = "Authorization")
            },
            operationId = "updateIbanMassive")
    @ApiResponses(
            value = {
                    @ApiResponse(responseCode = "200", description = "IBANs updated"),
                    @ApiResponse(
                            responseCode = "400",
                            description = "Malformed request.",
                            content =
                            @Content(
                                    mediaType = MediaType.APPLICATION_JSON_VALUE,
                                    schema = @Schema(implementation = ProblemJson.class))),
                    @ApiResponse(
                            responseCode = "401",
                            description = "Wrong or missing function key.",
                            content = @Content(schema = @Schema())),
                    @ApiResponse(
                            responseCode = "404",
                            description = "No valid IBAN found.",
                            content = @Content(schema = @Schema(implementation = ProblemJson.class))),
                    @ApiResponse(
                            responseCode = "422",
                            description = "Unprocessable: no payment position in valid state.",
                            content =
                            @Content(
                                    mediaType = MediaType.APPLICATION_JSON_VALUE,
                                    schema = @Schema(implementation = ProblemJson.class))),
                    @ApiResponse(
                            responseCode = "500",
                            description = "Service unavailable.",
                            content =
                            @Content(
                                    mediaType = MediaType.APPLICATION_JSON_VALUE,
                                    schema = @Schema(implementation = ProblemJson.class)))
            })
    @PostMapping(
            value = "/iban/{organizationfiscalcode}/update-massive",
            produces = {"application/json"},
            consumes = {"application/json"})
    void updateIbanMassive(
            @Parameter(
                    description = "Organization fiscal code, the fiscal code of the Organization.",
                    required = true)
            @PathVariable("organizationfiscalcode")
            String organizationFiscalCode,
            @Valid @RequestBody UpdateIbanMassiveModel updateIbanMassiveModel);
}
