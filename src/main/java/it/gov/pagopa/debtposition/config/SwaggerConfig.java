package it.gov.pagopa.debtposition.config;

import static it.gov.pagopa.debtposition.util.Constants.HEADER_REQUEST_ID;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import java.util.List;
import java.util.Map;
import org.springdoc.core.GroupedOpenApi;
import org.springdoc.core.customizers.OpenApiCustomiser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfig {

  @Bean
  public OpenAPI customOpenAPI(
      @Value("${info.application.description}") String appDescription,
      @Value("${info.application.version}") String appVersion) {
    // GPD servers
    Server GPD_UAT = new Server();
    GPD_UAT.setUrl("https://api.uat.platform.pagopa.it/gpd/debt-positions-service/v1/");
    GPD_UAT.setDescription("GPD Test environment");
    Server GPD_PROD = new Server();
    GPD_PROD.setUrl("GPD Production environment");
    GPD_PROD.setDescription("https://api.platform.pagopa.it/gpd/debt-positions-service/v1/");
    // GPD servers v3
    Server GPD_UAT_v3 = new Server();
    GPD_UAT_v3.setUrl("https://api.uat.platform.pagopa.it/gpd/debt-positions-service/v3");
    GPD_UAT_v3.setDescription("GPD Test environment");
    // ACA servers
    Server ACA_UAT = new Server();
    ACA_UAT.setUrl("https://api.uat.platform.pagopa.it/aca/debt-positions-service/v1/");
    ACA_UAT.setDescription("ACA Test environment");
    Server ACA_PROD = new Server();
    ACA_PROD.setUrl("https://api.platform.pagopa.it/aca/debt-positions-service/v1/");
    ACA_PROD.setDescription("ACA Production environment");

    return new OpenAPI()
        .components(
            new Components()
                .addSecuritySchemes(
                    "ApiKey",
                    new SecurityScheme()
                        .type(SecurityScheme.Type.APIKEY)
                        .description("The API key to access this function app.")
                        .name("Ocp-Apim-Subscription-Key")
                        .in(SecurityScheme.In.HEADER))
                .addSecuritySchemes(
                    "Authorization",
                    new SecurityScheme()
                        .type(SecurityScheme.Type.HTTP)
                        .description("JWT token get after Azure Login")
                        .name("Authorization")
                        .scheme("bearer")
                        .bearerFormat("JWT")
                        .in(SecurityScheme.In.HEADER)))
        .info(
            new Info()
                .title("PagoPA API Debt Position")
                .version(appVersion)
                .description(appDescription)
                .termsOfService("https://www.pagopa.gov.it/"))
        .servers(List.of(GPD_UAT, GPD_UAT_v3, GPD_PROD, ACA_UAT, ACA_PROD));
  }

  @Bean
  public OpenApiCustomiser sortOperationsAlphabetically() {
    return openApi -> {
      Paths paths =
          openApi.getPaths().entrySet().stream()
              .sorted(Map.Entry.comparingByKey())
              .collect(
                  Paths::new,
                  (map, item) -> map.addPathItem(item.getKey(), item.getValue()),
                  Paths::putAll);

      openApi.setPaths(paths);
    };
  }

  @Bean
  public Map<String, GroupedOpenApi> configureGroupOpenApi(
      Map<String, GroupedOpenApi> groupOpenApi) {
    groupOpenApi.forEach(
        (id, groupedOpenApi) -> groupedOpenApi.getOpenApiCustomisers().add(addCommonHeaders()));
    return groupOpenApi;
  }

  @Bean
  public OpenApiCustomiser addCommonHeaders() {
    return openApi ->
        openApi
            .getPaths()
            .forEach(
                (key, value) -> {

                  // add Request-ID as request header
                  value.addParametersItem(
                      new Parameter()
                          .in("header")
                          .name(HEADER_REQUEST_ID)
                          .schema(new StringSchema())
                          .description(
                              "This header identifies the call, if not passed it is self-generated."
                                  + " This ID is returned in the response."));

                  // add Request-ID as response header
                  value
                      .readOperations()
                      .forEach(
                          operation ->
                              operation
                                  .getResponses()
                                  .values()
                                  .forEach(
                                      response ->
                                          response.addHeaderObject(
                                              HEADER_REQUEST_ID,
                                              new Header()
                                                  .schema(new StringSchema())
                                                  .description(
                                                      "This header identifies the call"))));
                });
  }
}
