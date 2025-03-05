package it.gov.pagopa.debtposition.config;

import static it.gov.pagopa.debtposition.util.Constants.HEADER_REQUEST_ID;

import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.headers.Header;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Collectors;

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
        .servers(getServerInfo());
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

  @Bean
  public GroupedOpenApi internalV1Api() {

    Map<String, Set<String>> removeFromInternalV1 = Map.of(
            "/organizations/{organizationfiscalcode}/debtpositions", Set.of("put", "delete"),
            "/organizations/{organizationfiscalcode}/debtpositions/bulk", Set.of("post")
    );

    return GroupedOpenApi.builder()
            .group("internal_v1")
            .displayName("GPD - Internal API - v1")
            .pathsToMatch("/**/**")
            .pathsToExclude("/v3/**")
            .addOpenApiCustomiser(removeFromOpenApi(removeFromInternalV1))
            .build();
  }

  @Bean
  public GroupedOpenApi externalV3Api() {

    return GroupedOpenApi.builder()
            .group("external_v3")
            .displayName("GPD - External API: Installments and Payment Options Manager")
            .pathsToMatch("/v3/**")
            .addOpenApiCustomiser(sortOpenApi())
            .build();
  }

  @Bean
  public GroupedOpenApi externalMassiveApi() {

    return GroupedOpenApi.builder()
            .group("external_massive")
            .displayName("GPD - External API Massive")
            .pathsToMatch("/organizations/{organizationfiscalcode}/debtpositions/bulk,/info")
            .addOpenApiCustomiser(sortOpenApi())
            .build();
  }

  @Bean
  public GroupedOpenApi sendApi() {

    return GroupedOpenApi.builder()
            .group("send")
            .displayName("GPD API - SEND")
            .pathsToMatch("/organizations/{organizationfiscalcode}/paymentoptions/{iuv}/notificationfee,organizations/{organizationfiscalcode}/paymentoptions/{iuv},/info")
            .addOpenApiCustomiser(sortOpenApi())
            .build();
  }

  private Server createServer(String env, String service, String version, String description) {
    String baseUrl = "https://api%s.platform.pagopa.it/%s/debt-positions-service/%s/";
    String url = String.format(baseUrl, env, service, version);
    Server server = new Server();
    server.setUrl(url);
    server.setDescription(description);
    return server;
  }

  private List<Server> getServerInfo() {
    List<Server> serverInfo = new ArrayList<>();
    // Add GPD servers (v1 and v3)
    serverInfo.add(createServer(".uat", "gpd", "v1", "GPD Test environment"));
    serverInfo.add(createServer(".uat", "gpd", "v3", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd", "v1", "GPD Production Environment"));
    serverInfo.add(createServer("", "gpd", "v3", "GPD Production Environment"));
    // Add ACA servers (v1)
    serverInfo.add(createServer(".uat", "aca", "v1", "ACA Test environment"));
    serverInfo.add(createServer("", "aca", "v1", "ACA Production environment"));

    return serverInfo;
  }

  private OpenApiCustomiser sortOpenApi() {
    return openApi -> {
      if (openApi.getPaths() == null) return;

      // Ordina i path in ordine alfabetico
      sortPaths(openApi);

      // Ordina le risposte HTTP per ogni operazione
      sortResponses(openApi);
    };
  }

  private OpenApiCustomiser removeFromOpenApi(Map<String, Set<String>> pathsToRemove) {
    return openApi -> {
      if (openApi.getPaths() == null) return;

      // Percorsi da rimuovere completamente
      List<String> pathsToDelete = new ArrayList<>();

      pathsToRemove.forEach((path, methods) -> {
        PathItem pathItem = openApi.getPaths().get(path);
        if (pathItem != null) {

          // Rimuovi i metodi specificati
          methods.forEach(method -> {
            BiConsumer<PathItem, Operation> remover = getMethodRemovers().get(method.toLowerCase());
            if (remover != null) {
              remover.accept(pathItem, null);
            }
          });

          // Se il PathItem è vuoto dopo la rimozione, segnalalo per l'eliminazione
          if (isPathItemEmpty(pathItem)) {
            pathsToDelete.add(path);
          }
        }
      });

      // remove paths with no methods
      pathsToDelete.forEach(openApi.getPaths()::remove);

      // Ordina i path in ordine alfabetico
      sortPaths(openApi);

      // Ordina le risposte HTTP per ogni operazione
      sortResponses(openApi);
    };
  }

  private void sortPaths(OpenAPI openApi) {
    Map<String, PathItem> sortedPaths = openApi.getPaths().entrySet().stream()
            .sorted((entry1, entry2) -> entry1.getKey().compareTo(entry2.getKey())) // Lambda per ordinare le chiavi
            .collect(Collectors.toMap(
                    Map.Entry::getKey,
                    Map.Entry::getValue,
                    (e1, e2) -> e1,
                    LinkedHashMap::new
            ));

    // Aggiungi i path ordinati manualmente a Paths
    Paths paths = new Paths();
    sortedPaths.forEach(paths::put);
    openApi.setPaths(paths);
  }

  // sort HTTP responses according to its state
  private void sortResponses(OpenAPI openApi) {
    openApi.getPaths().values().forEach(this::sortOperationResponses);
  }

  // sort responses of each endpoint
  private void sortOperationResponses(PathItem pathItem) {
    List<Operation> operations = new ArrayList<>();

    // add operation not null
    if (pathItem.getGet() != null) operations.add(pathItem.getGet());
    if (pathItem.getPost() != null) operations.add(pathItem.getPost());
    if (pathItem.getPut() != null) operations.add(pathItem.getPut());
    if (pathItem.getDelete() != null) operations.add(pathItem.getDelete());
    if (pathItem.getPatch() != null) operations.add(pathItem.getPatch());
    if (pathItem.getHead() != null) operations.add(pathItem.getHead());
    if (pathItem.getOptions() != null) operations.add(pathItem.getOptions());
    if (pathItem.getTrace() != null) operations.add(pathItem.getTrace());

    for (Operation operation : operations) {
      if (operation != null && operation.getResponses() != null) {
        ApiResponses sortedResponses = operation.getResponses().entrySet().stream()
                .sorted(Comparator.comparingInt(e -> parseStatusCode(e.getKey())))
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        Map.Entry::getValue,
                        (e1, e2) -> e1,
                        ApiResponses::new
                ));
        operation.setResponses(sortedResponses);
      }
    }
  }

  // convert status to integer
  private int parseStatusCode(String status) {
    try {
      return Integer.parseInt(status);
    } catch (NumberFormatException e) {
      return Integer.MAX_VALUE;
    }
  }

  private boolean isPathItemEmpty(PathItem pathItem) {
    return pathItem.getGet() == null &&
            pathItem.getPost() == null &&
            pathItem.getPut() == null &&
            pathItem.getDelete() == null &&
            pathItem.getPatch() == null &&
            pathItem.getHead() == null &&
            pathItem.getOptions() == null &&
            pathItem.getTrace() == null;
  }

  private Map<String, BiConsumer<PathItem, Operation>> getMethodRemovers() {
    return Map.of(
            "get", PathItem::setGet,
            "post", PathItem::setPost,
            "put", PathItem::setPut,
            "delete", PathItem::setDelete,
            "patch", PathItem::setPatch,
            "head", PathItem::setHead,
            "options", PathItem::setOptions,
            "trace", PathItem::setTrace
    );
  }
}
