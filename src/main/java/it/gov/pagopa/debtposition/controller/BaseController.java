package it.gov.pagopa.debtposition.controller;

import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import it.gov.pagopa.debtposition.model.AppInfo;
import it.gov.pagopa.debtposition.model.ProblemJson;
import it.gov.pagopa.debtposition.model.config.Notice;
import it.gov.pagopa.debtposition.service.config.SendConfigurator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.view.RedirectView;

import jakarta.validation.constraints.Max;
import java.util.List;

@RestController()
public class BaseController {
    @Value("${info.application.name}")
    private String name;

    @Value("${info.application.version}")
    private String version;

    @Value("${info.properties.environment}")
    private String environment;

    private final SendConfigurator sendConfigurator;

    @Autowired
    public BaseController(SendConfigurator sendConfigurator) {
        this.sendConfigurator = sendConfigurator;
    }

    /**
    * @return 200 OK
    */
    @Hidden
    @GetMapping("")
    @ResponseStatus(HttpStatus.OK)
    public RedirectView home() {
        return new RedirectView("/swagger-ui/index.html");
    }

    /**
    * Health Check
    *
    * @return ok
    */
    @Operation(
      summary = "Return OK if application is started",
      security = {
        @SecurityRequirement(name = "ApiKey")
      },
      tags = {"Home"})
    @ApiResponses(
      value = {
        @ApiResponse(
            responseCode = "200",
            description = "OK.",
            content =
                @Content(
                    mediaType = MediaType.APPLICATION_JSON_VALUE,
                    schema = @Schema(implementation = AppInfo.class))),
        @ApiResponse(
            responseCode = "401",
            description = "Wrong or missing function key.",
            content = @Content(schema = @Schema())),
        @ApiResponse(
            responseCode = "403",
            description = "Forbidden.",
            content = @Content(schema = @Schema())),
        @ApiResponse(
            responseCode = "500",
            description = "Service unavailable.",
            content =
                @Content(
                    mediaType = MediaType.APPLICATION_JSON_VALUE,
                    schema = @Schema(implementation = ProblemJson.class)))
      })
    @GetMapping("/info")
    public ResponseEntity<AppInfo> healthCheck() {
    AppInfo info = AppInfo.builder().name(name).version(version).environment(environment).build();
        return ResponseEntity.status(HttpStatus.OK).body(info);
    }

    /**
    * Internal API for Synchronous SeND Configuration
    *
    * @return ok
    */
    @Operation(
          summary = "Configures payment options for which communication with SeND is synchronous",
          security = {
                  @SecurityRequirement(name = "ApiKey")
          },
          tags = {"Configuration"})
    @ApiResponses(
          value = {
                  @ApiResponse(
                          responseCode = "200",
                          description = "OK.",
                          content =
                          @Content(
                                  mediaType = MediaType.APPLICATION_JSON_VALUE,
                                  schema = @Schema(implementation = AppInfo.class))),
                  @ApiResponse(
                          responseCode = "401",
                          description = "Wrong or missing function key.",
                          content = @Content(schema = @Schema())),
                  @ApiResponse(
                          responseCode = "403",
                          description = "Forbidden.",
                          content = @Content(schema = @Schema())),
                  @ApiResponse(
                          responseCode = "500",
                          description = "Service unavailable.",
                          content =
                          @Content(
                                  mediaType = MediaType.APPLICATION_JSON_VALUE,
                                  schema = @Schema(implementation = ProblemJson.class)))
          })
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE, path = "internal/config/send")
    public ResponseEntity<String> handleSyncSendConfiguration(@RequestBody @Max(1000) List<Notice> notices) {
        int updated = sendConfigurator.updateSendSync(notices);
        return ResponseEntity.status(HttpStatus.OK).body("Configuration completed for " + updated + " items");
    }
}
