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
            "/organizations/{organizationfiscalcode}/debtpositions", Set.of("put", "delete"),
            "/organizations/{organizationfiscalcode}/debtpositions/bulk", Set.of("post")
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
            .addOpenApiCustomiser(customizeOpenApi(serverInfo, removeFromInternalV1))
            .build();
  }

  @Bean
  public GroupedOpenApi internalV2Api() {

    // api to remove
    Map<String, Set<String>> removeFromInternalV2 = Map.of();

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "gpd/api", "v2", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd/api", "v2", "GPD Production Environment"));

    return GroupedOpenApi.builder()
            .group("internal_v2")
            .displayName("GPD - Internal API - v2")
            .pathsToMatch("/**/**")
            .pathsToExclude("/v3/**")
            .addOpenApiCustomiser(customizeOpenApi(serverInfo, removeFromInternalV2))
            .build();
  }

  @Bean
  public GroupedOpenApi externalV1Api() {
    Map<String, Set<String>> removeFromExternalV1 = Map.of();

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "gpd/debt-positions-service", "v1", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd/debt-positions-service", "v1", "GPD Production Environment"));


    return GroupedOpenApi.builder()
            .group("external_v1")
            .displayName("GPD - External API - v1")
            .pathsToMatch("/organizations/{organizationfiscalcode}/debtpositions/**", "/info")
            .pathsToExclude("/organizations/{organizationfiscalcode}/debtpositions/bulk")
            .addOpenApiCustomiser(customizeOpenApi(serverInfo, removeFromExternalV1))
            .build();
  }

  @Bean
  public GroupedOpenApi externalV2Api() {
    Map<String, Set<String>> removeFromExternalV2 = Map.of(
            "/organizations/{organizationfiscalcode}/debtpositions", Set.of("get")
    );

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "gpd/debt-positions-service", "v2", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd/debt-positions-service", "v2", "GPD Production Environment"));


    return GroupedOpenApi.builder()
            .group("external_v2")
            .displayName("GPD - External API - v2")
            .pathsToMatch("/organizations/{organizationfiscalcode}/debtpositions", "/info")
            .addOpenApiCustomiser(customizeOpenApi(serverInfo, removeFromExternalV2))
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
            .displayName("GPD - External API: Installments and Payment Options Manager")
            .pathsToMatch("/v3/**")
            .addOpenApiCustomiser(customizeOpenApi(serverInfo, removeFromExternalV3))
            .build();
  }

  @Bean
  public GroupedOpenApi sendV1Api() {
    Map<String, Set<String>> removeFromSendV1 = Map.of();

    // server list
    List<Server> serverInfo = new ArrayList<>();
    serverInfo.add(createServer(".uat", "gpd/debt-positions-service", "v3", "GPD Test environment"));
    serverInfo.add(createServer("", "gpd/debt-positions-service", "v3", "GPD Production Environment"));

    return GroupedOpenApi.builder()
            .group("send_v1")
            .displayName("GPD - Send API - v1")
            .pathsToMatch("/organizations/{organizationfiscalcode}/paymentoptions/{iuv}/notificationfee","/organizations/{organizationfiscalcode}/paymentoptions/{iuv}","/info")
            .addOpenApiCustomiser(customizeOpenApi(serverInfo, removeFromSendV1))
            .build();
  }


  private Server createServer(String env, String service, String version, String description) {
    String baseUrl = "https://api%s.platform.pagopa.it/%s/%s/";
    String url = String.format(baseUrl, env, service, version);
    Server server = new Server();
    server.setUrl(url);
    server.setDescription(description);
    return server;
  }

  private OpenApiCustomiser customizeOpenApi(List<Server> serverInfo, Map<String, Set<String>> pathsToRemove) {
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

          // if the path is empry then remove all
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

      // sort paths
      sortPaths(openApi);

      // sort HTTP responses
      sortResponses(openApi);

      // sort methods
      openApi.getPaths().forEach((path, pathItem) -> sortPathItemMethods(pathItem));

      // set servers
      openApi.setServers(serverInfo);
    };
  }

  private void sortPathItemMethods(PathItem pathItem) {
    Map<String, Operation> operationsMap = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

    if (pathItem.getDelete() != null) operationsMap.put("delete", pathItem.getDelete());
    if (pathItem.getGet() != null) operationsMap.put("get", pathItem.getGet());
    if (pathItem.getPatch() != null) operationsMap.put("patch", pathItem.getPatch());
    if (pathItem.getPost() != null) operationsMap.put("post", pathItem.getPost());
    if (pathItem.getPut() != null) operationsMap.put("put", pathItem.getPut());
    if (pathItem.getHead() != null) operationsMap.put("head", pathItem.getHead());
    if (pathItem.getOptions() != null) operationsMap.put("options", pathItem.getOptions());
    if (pathItem.getTrace() != null) operationsMap.put("trace", pathItem.getTrace());

    PathItem sortedPathItem = new PathItem();

    operationsMap.forEach((method, operation) -> {
      switch (method) {
        case "delete" -> sortedPathItem.setDelete(operation);
        case "get" -> sortedPathItem.setGet(operation);
        case "head" -> sortedPathItem.setHead(operation);
        case "options" -> sortedPathItem.setOptions(operation);
        case "patch" -> sortedPathItem.setPatch(operation);
        case "post" -> sortedPathItem.setPost(operation);
        case "put" -> sortedPathItem.setPut(operation);
        case "trace" -> sortedPathItem.setTrace(operation);
      }
    });

    //copy details
    sortedPathItem.setSummary(pathItem.getSummary());
    sortedPathItem.setDescription(pathItem.getDescription());
    sortedPathItem.setParameters(pathItem.getParameters());
    sortedPathItem.setServers(pathItem.getServers());
    sortedPathItem.setExtensions(pathItem.getExtensions());

    // replace pathItem
    pathItem.setDelete(sortedPathItem.getDelete());
    pathItem.setGet(sortedPathItem.getGet());
    pathItem.setHead(sortedPathItem.getHead());
    pathItem.setOptions(sortedPathItem.getOptions());
    pathItem.setPatch(sortedPathItem.getPatch());
    pathItem.setPost(sortedPathItem.getPost());
    pathItem.setPut(sortedPathItem.getPut());
    pathItem.setTrace(sortedPathItem.getTrace());
  }

  private List<Operation> getAllOperations(PathItem pathItem) {
    List<Operation> operations = new ArrayList<>();
    if (pathItem.getGet() != null) operations.add(pathItem.getGet());
    if (pathItem.getPost() != null) operations.add(pathItem.getPost());
    if (pathItem.getPut() != null) operations.add(pathItem.getPut());
    if (pathItem.getDelete() != null) operations.add(pathItem.getDelete());
    if (pathItem.getPatch() != null) operations.add(pathItem.getPatch());
    if (pathItem.getHead() != null) operations.add(pathItem.getHead());
    if (pathItem.getOptions() != null) operations.add(pathItem.getOptions());
    if (pathItem.getTrace() != null) operations.add(pathItem.getTrace());
    return operations;
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
