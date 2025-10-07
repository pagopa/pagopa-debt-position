package it.gov.pagopa.debtposition.controller.pd.crud.api.v2;

import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import it.gov.pagopa.debtposition.model.ProblemJson;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.pd.*;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.*;

@Tag(name = "Debt Positions API - v2")
@Validated
@RequestMapping
public interface IDebtPositionControllerv2 {

  // Only for gpd-core-internal
  // External client calls the v2 api exposed on APIM through v1 endpoints

  @Operation(
      summary = "The Organization creates multiple debt positions.",
      security = {
        @SecurityRequirement(name = "ApiKey")
      },
      operationId = "createMultiplePositions")
  @ApiResponses(
      value = {
        @ApiResponse(responseCode = "201", description = "Request created."),
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
            responseCode = "403",
            content = @Content(schema = @Schema(), examples = {@ExampleObject(value = """
                {
                    "statusCode": 403,
                    "message": "You are not allowed to access this resource."
                }""")}, mediaType = MediaType.APPLICATION_JSON_VALUE)),
        @ApiResponse(
            responseCode = "409",
            description = "Conflict: duplicate debt position found.",
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
      value = "/v2/organizations/{organizationfiscalcode}/debtpositions",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Hidden
  ResponseEntity<Void> createMultipleDebtPositions(
      @Parameter(
              description = "Organization fiscal code, the fiscal code of the Organization.",
              required = true)
          @PathVariable("organizationfiscalcode")
          String organizationFiscalCode,
      @Valid @RequestBody MultiplePaymentPositionModel multiplePaymentPositionModel,
      @RequestParam(required = false, defaultValue = "false") boolean toPublish,
      @Valid
          @Parameter(
              description = "Segregation codes for which broker is authorized",
              hidden = true)
          @Pattern(regexp = "\\d{2}(,\\d{2})*")
          @RequestParam(required = false)
          String segregationCodes,
      @Parameter(
              hidden = false,
              description =
                  "The field must not be considered as its value is set via the API Management"
                      + " (APIM) policy")
          @RequestParam(required = false, defaultValue = "GPD")
          ServiceType serviceType);

  @Operation(
      summary = "The Organization updates multiple debt positions.",
      security = {
        @SecurityRequirement(name = "ApiKey")
      },
      operationId = "updateMultiplePositions")
  @ApiResponses(
      value = {
        @ApiResponse(responseCode = "200", description = "Debt Positions updated."),
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
            responseCode = "403",
            content =
                @Content(schema = @Schema(), examples = {@ExampleObject(value = """
                    {
                        "statusCode": 403,
                        "message": "You are not allowed to access this resource."
                    }""")}, mediaType = MediaType.APPLICATION_JSON_VALUE)),
        @ApiResponse(
            responseCode = "409",
            description = "Conflict: existing related payment found.",
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
  @PutMapping(
      value = "/v2/organizations/{organizationfiscalcode}/debtpositions",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Hidden
  ResponseEntity<Void> updateMultipleDebtPositions(
      @Parameter(
              description = "Organization fiscal code, the fiscal code of the Organization.",
              required = true)
          @PathVariable("organizationfiscalcode")
          String organizationFiscalCode,
      @Valid @RequestBody MultiplePaymentPositionModel multiplePaymentPositionModel,
      @RequestParam(required = false, defaultValue = "false") boolean toPublish,
      @Valid
          @Parameter(
              description = "Segregation codes for which broker is authorized",
              hidden = true)
          @Pattern(regexp = "\\d{2}(,\\d{2})*")
          @RequestParam(required = false)
          String segregationCodes);

  @Operation(
      summary = "The Organization deletes multiple debt positions.",
      security = {
        @SecurityRequirement(name = "ApiKey")
      },
      operationId = "deleteMultipleDebtPositions")
  @ApiResponses(
      value = {
        @ApiResponse(responseCode = "200", description = "Debt Positions deleted."),
        @ApiResponse(
            responseCode = "400",
            description = "Malformed request.",
            content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
        @ApiResponse(
            responseCode = "401",
            description = "Wrong or missing function key.",
            content = @Content(schema = @Schema())),
        @ApiResponse(
            responseCode = "403",
            content =
                @Content(schema = @Schema(), examples = {@ExampleObject(value = """
                    {
                        "statusCode": 403,
                        "message": "You are not allowed to access this resource."
                    }""")}, mediaType = MediaType.APPLICATION_JSON_VALUE)),
        @ApiResponse(
            responseCode = "404",
            description = "Payment Position not found.",
            content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE)),
        @ApiResponse(
            responseCode = "500",
            description = "Service unavailable.",
            content = @Content(mediaType = MediaType.APPLICATION_JSON_VALUE))
      })
  @DeleteMapping(
      value = "/v2/organizations/{organizationfiscalcode}/debtpositions",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Hidden
  ResponseEntity<String> deleteMultipleDebtPositions(
      @Parameter(
              description = "Organization fiscal code, the fiscal code of the Organization.",
              required = true)
          @Pattern(regexp = "\\b\\w{11}\\b")
          @PathVariable("organizationfiscalcode")
          String organizationFiscalCode,
      @Valid @RequestBody MultipleIUPDModel multipleIUPDModel,
      @Valid
          @Parameter(
              description = "Segregation codes for which broker is authorized",
              hidden = true)
          @Pattern(regexp = "\\d{2}(,\\d{2})*")
          @RequestParam(required = false)
          String segregationCodes);
}
