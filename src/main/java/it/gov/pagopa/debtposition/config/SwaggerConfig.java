package it.gov.pagopa.debtposition.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springdoc.core.GroupedOpenApi;
import org.springdoc.core.customizers.OpenApiCustomiser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Map;

import static it.gov.pagopa.debtposition.util.Constants.HEADER_REQUEST_ID;

@Configuration
public class SwaggerConfig {

    @Bean
    public OpenAPI customOpenAPI(@Value("${info.application.description}") String appDescription, @Value("${info.application.version}") String appVersion) {
        return new OpenAPI()
                .components(new Components()
                        .addSecuritySchemes("ApiKey", new SecurityScheme()
                                .type(SecurityScheme.Type.APIKEY)
                                .description("The API key to access this function app.")
                                .name("Ocp-Apim-Subscription-Key")
                                .in(SecurityScheme.In.HEADER))
                        .addSecuritySchemes("Authorization",
                                new SecurityScheme()
                                        .type(SecurityScheme.Type.HTTP)
                                        .description("JWT token get after Azure Login")
                                        .name("Authorization")
                                        .scheme("bearer")
                                        .bearerFormat("JWT")
                                        .in(SecurityScheme.In.HEADER))
                )
                .info(new Info()
                        .title("PagoPA API Debt Position")
                        .version(appVersion)
                        .description(appDescription)
                        .termsOfService("https://www.pagopa.gov.it/"));

    }

    @Bean
    public OpenApiCustomiser sortOperationsAlphabetically() {
        return openApi -> {
            Paths paths = openApi.getPaths().entrySet()
                    .stream()
                    .sorted(Map.Entry.comparingByKey())
                    .collect(Paths::new, (map, item) -> map.addPathItem(item.getKey(), item.getValue()), Paths::putAll);

            openApi.setPaths(paths);
        };
    }

    @Bean
    public Map<String, GroupedOpenApi> configureGroupOpenApi(Map<String, GroupedOpenApi> groupOpenApi) {
        groupOpenApi.forEach((id, groupedOpenApi) -> groupedOpenApi
                                                                .getOpenApiCustomisers()
                                                                .add(addCommonHeaders()));
        return groupOpenApi;
    }

    @Bean
    public OpenApiCustomiser addCommonHeaders() {
        return openApi -> openApi.getPaths().forEach((key, value) -> {


            // add Request-ID as request header
            value.addParametersItem(new Parameter().in("header")
                    .name(HEADER_REQUEST_ID)
                    .schema(new StringSchema())
                    .description("This header identifies the call, if not passed it is self-generated. This ID is returned in the response."));

            // add Request-ID as response header
            value.readOperations()
                    .forEach(operation -> operation.getResponses().values()
                            .forEach(response -> response.addHeaderObject(HEADER_REQUEST_ID, new Header()
                                    .schema(new StringSchema())
                                    .description("This header identifies the call"))));
        });
    }

}
