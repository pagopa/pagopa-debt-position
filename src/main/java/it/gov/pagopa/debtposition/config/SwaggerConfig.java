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
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springdoc.core.GroupedOpenApi;
import org.springdoc.core.customizers.OpenApiCustomiser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfig {

  private final static String INFO_API = "/info";
  private final static String DEBT_POSITIONS_API = "/organizations/{organizationfiscalcode}/debtpositions";
  private final static String DEBT_POSITIONS_BULK_API= "/organizations/{organizationfiscalcode}/debtpositions/bulk";
  private final static String PAYMENTS_MARK_AS_PAID_API = "/organizations/{organizationfiscalcode}/paymentoptions/paids/{nav}";
  private final static String DEBT_POSITION_API_BLOCK = "/organizations/{organizationfiscalcode}/debtpositions/**";

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
        )
        .info(
            new Info()
                .title("PagoPA API Debt Position ${service}")
                .version(appVersion)
                .description(appDescription)
                .termsOfService("https://www.pagopa.gov.it/"));
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

    // api to remove
    Map<String, Set<String>> removeFromInternalV1 = Map.of(
            DEBT_POSITIONS_API, Set.of("put", "delete"),
            DEBT_POSITIONS_BULK_API, Set.of("post")
    );

    Set<String> schemasToRemove = Set.of(
            "MultiplePaymentPositionModel",
            "MultipleIUPDModel"
    );

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "gpd/api", "v1", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd/api", "v1", "GPD Production Environment"));

    return GroupedOpenApi.builder()
            .group("internal_v1")
            .displayName("GPD - Internal API - v1")
            .pathsToMatch("/**/**")
            .pathsToExclude("/v3/**")
            .addOpenApiCustomiser(customizeServer(serverInfo))
            .addOpenApiCustomiser(customizeOpenApi(removeFromInternalV1))
            .addOpenApiCustomiser(removeSchema(schemasToRemove))
            .addOpenApiCustomiser(sortOpenApi())
            .build();
  }

  @Bean
  public GroupedOpenApi internalV2Api() {

    // api to remove
    Map<String, Set<String>> removeFromInternalV2 = Map.of(
            DEBT_POSITIONS_API, Set.of("post")
    );

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "gpd/api", "v2", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd/api", "v2", "GPD Production Environment"));

    GroupedOpenApi openapi = GroupedOpenApi.builder()
            .group("internal_v2")
            .displayName("GPD - Internal API - v2")
            .pathsToMatch("/**/**")
            .pathsToExclude("/v3/**")
            .addOpenApiCustomiser(customizeServer(serverInfo))
            .addOpenApiCustomiser(customizeOpenApi(removeFromInternalV2))
            .addOpenApiCustomiser(renamePath(DEBT_POSITIONS_BULK_API, DEBT_POSITIONS_API))
            .addOpenApiCustomiser(sortOpenApi())
            .build();

    return openapi;
  }

  @Bean
  public GroupedOpenApi externalV1Api() {
    Map<String, Set<String>> removeFromExternalV1 = Map.of(
            DEBT_POSITIONS_API, Set.of("put", "delete")
    );

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "gpd/debt-positions-service", "v1", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd/debt-positions-service", "v1", "GPD Production Environment"));

    Set<String> schemasToRemove = Set.of(
            "MultiplePaymentPositionModel",
            "MultipleIUPDModel"
    );

    return GroupedOpenApi.builder()
            .group("external_v1")
            .displayName("GPD - External API - v1")
            .pathsToMatch(DEBT_POSITION_API_BLOCK, PAYMENTS_MARK_AS_PAID_API, INFO_API)
            .pathsToExclude(DEBT_POSITIONS_BULK_API)
            .addOpenApiCustomiser(customizeServer(serverInfo))
            .addOpenApiCustomiser(customizeOpenApi(removeFromExternalV1))
            .addOpenApiCustomiser(removeSchema(schemasToRemove))
            .addOpenApiCustomiser(sortOpenApi())
            .build();
  }

  @Bean
  public GroupedOpenApi externalV2Api() {
    Map<String, Set<String>> removeFromExternalV2 = Map.of(
            DEBT_POSITIONS_API, Set.of("get", "post")
    );

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "gpd/debt-positions-service", "v2", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd/debt-positions-service", "v2", "GPD Production Environment"));

    return GroupedOpenApi.builder()
            .group("external_v2")
            .displayName("GPD - External API - v2")
            .pathsToMatch(DEBT_POSITIONS_API, DEBT_POSITIONS_BULK_API, PAYMENTS_MARK_AS_PAID_API, INFO_API)
            .addOpenApiCustomiser(customizeServer(serverInfo))
            .addOpenApiCustomiser(customizeOpenApi(removeFromExternalV2))
            .addOpenApiCustomiser(renamePath(DEBT_POSITIONS_BULK_API, DEBT_POSITIONS_API))
            .addOpenApiCustomiser(sortOpenApi())
            .build();
  }

  @Bean
  public GroupedOpenApi externalV3Api() {
    Map<String, Set<String>> removeFromExternalV3 = Map.of();

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "gpd/debt-positions-service", "v3", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd/debt-positions-service", "v3", "GPD Production Environment"));

    return GroupedOpenApi.builder()
            .group("external_v3")
            .displayName("GPD - External API - v3")
            .pathsToMatch("/v3/**")
            .addOpenApiCustomiser(customizeServer(serverInfo))
            .addOpenApiCustomiser(customizeOpenApi(removeFromExternalV3))
            .addOpenApiCustomiser(removePrefixFromPaths("/v3"))
            .addOpenApiCustomiser(sortOpenApi())
            .build();
  }

  @Bean
  public GroupedOpenApi acaV1Api() {
    Map<String, Set<String>> removeFromAcaV1 = Map.of(
            DEBT_POSITIONS_API, Set.of("put", "delete"),
            "/organizations/{organizationfiscalcode}/debtpositions/transfers", Set.of("patch")
    );

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "aca/debt-positions-service", "v1", "ACA Test environment"));
    serverInfo.add(createServer("", "aca/debt-positions-service", "v1", "ACA Production Environment"));

    Set<String> schemasToRemove = Set.of(
            "MultiplePaymentPositionModel",
            "MultipleIUPDModel",
            "UpdateTransferIbanMassiveModel",
            "UpdateTransferIbanMassiveResponse"
    );

    return GroupedOpenApi.builder()
            .group("aca_v1")
            .displayName("GPD - ACA API - v1")
            .pathsToMatch(DEBT_POSITION_API_BLOCK, PAYMENTS_MARK_AS_PAID_API, INFO_API)
            .pathsToExclude(DEBT_POSITIONS_BULK_API)
            .addOpenApiCustomiser(customizeServer(serverInfo))
            .addOpenApiCustomiser(customizeOpenApi(removeFromAcaV1))
            .addOpenApiCustomiser(removeSchema(schemasToRemove))
            .addOpenApiCustomiser(sortOpenApi())
            .build();
  }

  @Bean
  public GroupedOpenApi sendV1Api() {
    Map<String, Set<String>> removeFromSendV1 = Map.of();

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "pn-integration-gpd/api", "v1", "GPD Test environment"));
    serverInfo.add(createServer("", "pn-integration-gpd/api", "v1", "GPD Production Environment"));

    return GroupedOpenApi.builder()
            .group("send_v1")
            .displayName("GPD - Send API - v1")
            .pathsToMatch("/organizations/{organizationfiscalcode}/paymentoptions/{iuv}/notificationfee","/organizations/{organizationfiscalcode}/paymentoptions/{iuv}", INFO_API)
            .addOpenApiCustomiser(customizeServer(serverInfo))
            .addOpenApiCustomiser(customizeOpenApi(removeFromSendV1))
            .addOpenApiCustomiser(sortOpenApi())
            .build();
  }

  private Server createServer(String env, String service, String version, String description) {
    String baseUrl = "https://api%s.platform.pagopa.it/%s";
    String url = String.format(baseUrl, env, service);
    if (version != null) {
      url = String.format("%s/%s", url, version);
    }
    Server server = new Server();
    server.setUrl(url);
    server.setDescription(description);
    return server;
  }

  private OpenApiCustomiser customizeServer(List<Server> serverInfo) {
    return openApi -> {
      if (openApi.getPaths() == null) return;

       // set servers
      openApi.setServers(serverInfo);
    };
  }

  private OpenApiCustomiser removePrefixFromPaths(String prefix) {
    return openApi -> {
      if (openApi.getPaths() == null) {
        return;
      }

      Map<String, PathItem> updatedPaths = new LinkedHashMap<>();

      openApi.getPaths().forEach((path, pathItem) -> {
        String newPath = path.startsWith(prefix) ? path.substring(prefix.length()) : path;

        updatedPaths.merge(newPath, pathItem, (target, source) -> {
          mergePathItems(target, source);
          return target;
        });

        if (!updatedPaths.isEmpty()) {
          openApi.setPaths(new Paths());
          updatedPaths.forEach((p, pi) -> openApi.getPaths().addPathItem(p, pi));
        }
      });
    };
  }

  private OpenApiCustomiser renamePath(String oldPath, String newPath) {
    return openApi -> {
      if (openApi.getPaths() == null || !openApi.getPaths().containsKey(oldPath)) {
        return; // Esce se il vecchio path non esiste
      }

      PathItem oldPathItem = openApi.getPaths().remove(oldPath); // Rimuove il path originale
      PathItem newPathItem = openApi.getPaths().getOrDefault(newPath, new PathItem());

      // Unisce le operazioni senza sovrascrivere quelle esistenti
      mergePathItems(newPathItem, oldPathItem);

      openApi.getPaths().addPathItem(newPath, newPathItem); // Aggiunge il path aggiornato
    };
  }

  private void mergePathItems(PathItem target, PathItem source) {
    Arrays.stream(PathItem.HttpMethod.values()).forEach(method -> {
      if (source.readOperationsMap().containsKey(method) && !target.readOperationsMap().containsKey(method)) {
        target.operation(method, source.readOperationsMap().get(method));
      }
    });
  }

  private OpenApiCustomiser customizeOpenApi(Map<String, Set<String>> pathsToRemove) {
    return openApi -> {
      if (openApi.getPaths() == null) return;

      // paths to remove
      List<String> pathsToDelete = new ArrayList<>();

      pathsToRemove.forEach((path, methods) -> {
        PathItem pathItem = openApi.getPaths().get(path);
        if (pathItem != null) {

          // remove specified methods
          methods.forEach(method -> {
            BiConsumer<PathItem, Operation> remover = getMethodRemovers().get(method.toLowerCase());
            if (remover != null) {
              remover.accept(pathItem, null);
            }
          });

          // if the path is empty then remove all
          if (isPathItemEmpty(pathItem)) {
            pathsToDelete.add(path);
          }
        }
      });

      // remove paths with no methods
      pathsToDelete.forEach(openApi.getPaths()::remove);

      openApi.getPaths().values().forEach(pathItem -> {
        // remove serviceType from parameters
        List<Operation> operations = getAllOperations(pathItem);
        operations.forEach(this::removeServiceType);
      });
    };
  }

  private OpenApiCustomiser removeSchema(Set<String> schemasToRemove) {
    return openApi -> {
      if (openApi.getPaths() == null) return;

      Map<String, io.swagger.v3.oas.models.media.Schema> schemaMap = openApi.getComponents().getSchemas();

      // remove schemas
      schemasToRemove.forEach(schemaMap::remove);
    };
  }

  private OpenApiCustomiser sortOpenApi() {
    return openApi -> {
      if (openApi.getPaths() == null) return;

      // sort paths
      sortPaths(openApi);

      // sort HTTP responses
      sortResponses(openApi);
    };
  }

  private List<Operation> getAllOperations(PathItem pathItem) {
    return Stream.of(
                    pathItem.getGet(),
                    pathItem.getPost(),
                    pathItem.getPut(),
                    pathItem.getDelete(),
                    pathItem.getPatch(),
                    pathItem.getHead(),
                    pathItem.getOptions(),
                    pathItem.getTrace()
            ).filter(Objects::nonNull)
            .toList();
  }

  private void removeServiceType(Operation operation) {
    if (operation.getParameters() != null) {
      // remove serviceType from parameters
      operation.setParameters(
              operation.getParameters().stream()
                      .filter(param -> !"serviceType".equals(param.getName()))
                      .collect(Collectors.toList())
      );
    }
  }

  private void sortPaths(OpenAPI openApi) {
    Map<String, PathItem> sortedPaths = openApi.getPaths().entrySet().stream()
            .sorted(Map.Entry.comparingByKey())
            .collect(Collectors.toMap(
                    Map.Entry::getKey,
                    Map.Entry::getValue,
                    (e1, e2) -> e1,
                    LinkedHashMap::new
            ));

    Paths paths = new Paths();
    sortedPaths.forEach(paths::addPathItem);
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
