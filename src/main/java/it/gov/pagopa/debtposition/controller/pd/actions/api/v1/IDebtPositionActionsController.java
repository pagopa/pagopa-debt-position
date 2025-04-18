package it.gov.pagopa.debtposition.controller.pd.actions.api.v1;

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
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import javax.validation.Valid;
import javax.validation.constraints.Pattern;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

@Tag(name = "Debt Position Actions API")
@RequestMapping
public interface IDebtPositionActionsController {

  @Operation(
      summary = "The Organization publish a debt Position.",
      security = {
        @SecurityRequirement(name = "ApiKey")
      },
      operationId = "publishPosition")
  @ApiResponses(
      value = {
        @ApiResponse(responseCode = "200", description = "Request published."),
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
            responseCode = "404",
            description = "No debt position found.",
            content = @Content(schema = @Schema(implementation = ProblemJson.class))),
        @ApiResponse(
            responseCode = "409",
            description = "Conflict: debt position is not in publishable state.",
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
      value = "/organizations/{organizationfiscalcode}/debtpositions/{iupd}/publish",
      produces = {"application/json"})
  ResponseEntity<PaymentPositionModel> publishDebtPosition(
      @Parameter(
              description = "Organization fiscal code, the fiscal code of the Organization.",
              required = true)
          @PathVariable("organizationfiscalcode")
          String organizationFiscalCode,
      @Parameter(
              description =
                  "IUPD (Unique identifier of the debt position). Format could be `<Organization"
                      + " fiscal code + UUID>` this would make it unique within the new PD"
                      + " management system. It's the responsibility of the EC to guarantee"
                      + " uniqueness. The pagoPa system shall verify that this is `true` and if"
                      + " not, notify the EC.",
              required = true)
          @PathVariable("iupd")
          String iupd,
      @Valid
          @Parameter(
              description = "Segregation codes for which broker is authorized",
              hidden = true)
          @Pattern(regexp = "\\d{2}(,\\d{2})*")
          @RequestParam(required = false)
          String segregationCodes);

  @Operation(
      summary = "The Organization invalidate a debt Position.",
      security = {
        @SecurityRequirement(name = "ApiKey")
      },
      operationId = "invalidatePosition")
  @ApiResponses(
      value = {
        @ApiResponse(responseCode = "200", description = "Request published."),
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
            responseCode = "404",
            description = "No debt position found.",
            content = @Content(schema = @Schema(implementation = ProblemJson.class))),
        @ApiResponse(
            responseCode = "409",
            description = "Conflict: debt position is not in invalidable state.",
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
      value = "/organizations/{organizationfiscalcode}/debtpositions/{iupd}/invalidate",
      produces = {"application/json"})
  ResponseEntity<PaymentPositionModel> invalidateDebtPosition(
      @Parameter(
              description = "Organization fiscal code, the fiscal code of the Organization.",
              required = true)
          @PathVariable("organizationfiscalcode")
          String organizationFiscalCode,
      @Parameter(
              description =
                  "IUPD (Unique identifier of the debt position). Format could be `<Organization"
                      + " fiscal code + UUID>` this would make it unique within the new PD"
                      + " management system. It's the responsibility of the EC to guarantee"
                      + " uniqueness. The pagoPa system shall verify that this is `true` and if"
                      + " not, notify the EC.",
              required = true)
          @PathVariable("iupd")
          String iupd,
      @Valid
          @Parameter(
              description = "Segregation codes for which broker is authorized",
              hidden = true)
          @Pattern(regexp = "\\d{2}(,\\d{2})*")
          @RequestParam(required = false)
          String segregationCodes);
}
