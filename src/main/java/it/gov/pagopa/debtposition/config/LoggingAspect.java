package it.gov.pagopa.debtposition.config;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import it.gov.pagopa.debtposition.config.logging.FailureLogger;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.ProblemJson;
import it.gov.pagopa.debtposition.util.CommonUtil;
import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.UUID;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.CodeSignature;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

@Aspect
@Component
@Slf4j
public class LoggingAspect {

  public static final String START_TIME = "startTime";
  public static final String METHOD = "method";
  public static final String STATUS = "status";
  public static final String CODE = "httpCode";
  public static final String RESPONSE_TIME = "responseTime";
  public static final String FAULT_CODE = "faultCode";
  public static final String FAULT_DETAIL = "faultDetail";
  public static final String REQUEST_ID = "requestId";
  public static final String OPERATION_ID = "operationId";
  public static final String ARGS = "args";

  private static final Set<String> EXCLUDED_ARGS =
      Set.of(
          "fiscalCode",
          "streetName",
          "civicNumber",
          "postalCode",
          "city",
          "province",
          "region",
          "country",
          "fullName",
          "email",
          "phone");

  /** Business identifiers searched at any depth of the inputs and of the response. */
  private static final Map<String, String> CTX_NESTED_FIELDS =
      Map.of(
          "nav", "ctx_nav",
          "iupd", "ctx_iupd",
          "iuv", "ctx_iuv",
          "paymentPositionIUPDs", "ctx_iupd");

  /**
   * Business identifiers (ctx_*) and call arguments (arguments.*) taken only from the API
   * parameters or the top level of a request body.
   */
  private static final Map<String, String> CTX_TOP_LEVEL_FIELDS =
      Map.of(
          "organizationFiscalCode", "ctx_organization_fiscal_code",
          "serviceType", "ctx_service_type",
          "oldIban", "ctx_old_iban",
          "newIban", "ctx_new_iban",
          "toPublish", "arguments.toPublish");

  private static final String NAV_FIELD = "nav";
  private static final String VALIDITY_DATE_FIELD = "validityDate";
  private static final String NAV_VALIDITY_KEY = "ctx_nav_validity_date";
  private static final String API_VERSION_KEY = "ctx_api_version";
  private static final Pattern VERSION_PREFIX = Pattern.compile("^/(v\\d+)(/|$)");
  private static final String DEFAULT_API_VERSION = "v1";

  /**
   * Parameters and request body properties logged as arguments.* only for the listed API methods.
   */
  private static final Map<String, Map<String, String>> ARGUMENTS_BY_METHOD =
      Map.of(
          "payPaymentOption", Map.of("paymentDate", "arguments.paymentDate"),
          "setPaymentOptionAsAlreadyPaid", Map.of("paymentDate", "arguments.paymentDate"),
          "reportTransfer",
              Map.of("transferId", "arguments.transferId", "iur", "arguments.iur"));

  private static final int MAX_LOGGED_VALUES = 100;

  private static final ObjectMapper ARGS_MAPPER =
      new ObjectMapper()
          .findAndRegisterModules()
          .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

  @Autowired HttpServletRequest httRequest;

  @Autowired HttpServletResponse httpResponse;

  @Value("${info.application.name}")
  private String name;

  @Value("${info.application.version}")
  private String version;

  @Value("${info.properties.environment}")
  private String environment;

  private static String getDetail(ResponseEntity<ProblemJson> result) {
    if (result != null && result.getBody() != null && result.getBody().getDetail() != null) {
      return result.getBody().getDetail();
    } else return AppError.UNKNOWN.getDetails();
  }

  private static String getTitle(ResponseEntity<ProblemJson> result) {
    if (result != null && result.getBody() != null && result.getBody().getTitle() != null) {
      return result.getBody().getTitle();
    } else return AppError.UNKNOWN.getTitle();
  }

  public static String getExecutionTime() {
    String startTime = MDC.get(START_TIME);
    if (startTime != null) {
      long endTime = System.currentTimeMillis();
      long executionTime = endTime - Long.parseLong(startTime);
      return String.valueOf(executionTime);
    }
    return "-";
  }

  private static Map<String, String> getParams(ProceedingJoinPoint joinPoint) {
    CodeSignature codeSignature = (CodeSignature) joinPoint.getSignature();
    Map<String, String> params = new HashMap<>();
    int i = 0;
    for (var paramName : codeSignature.getParameterNames()) {
      Object arg = joinPoint.getArgs()[i++];
      if (!EXCLUDED_ARGS.contains(paramName)) {
        params.put(paramName, CommonUtil.deNull(arg));
      }
    }
    return params;
  }

  /** Collects the business identifiers (ctx_*) from the API parameters. */
  private static void collectContextFromArguments(
      ProceedingJoinPoint joinPoint, Map<String, Set<String>> context) {
    String[] names = ((CodeSignature) joinPoint.getSignature()).getParameterNames();
    Object[] values = joinPoint.getArgs();
    Map<String, String> methodArguments =
        ARGUMENTS_BY_METHOD.getOrDefault(joinPoint.getSignature().getName(), Map.of());
    for (int i = 0; i < names.length; i++) {
      try {
        JsonNode node = ARGS_MAPPER.valueToTree(values[i]);
        collectNested(node, context);
        if (node.isObject()) {
          CTX_TOP_LEVEL_FIELDS.forEach((field, key) -> addValue(context, key, node.get(field)));
          methodArguments.forEach((field, key) -> addValue(context, key, node.get(field)));
        } else {
          addValue(context, CTX_TOP_LEVEL_FIELDS.get(names[i]), node);
          addValue(context, CTX_NESTED_FIELDS.get(names[i]), node);
          addValue(context, methodArguments.get(names[i]), node);
        }
      } catch (IllegalArgumentException e) {
        // argument not serializable: skipped
      }
    }
  }

  private static void collectContextFromResult(Object result, Map<String, Set<String>> context) {
    Object body = result instanceof ResponseEntity<?> entity ? entity.getBody() : result;
    if (body == null) {
      return;
    }
    try {
      collectNested(ARGS_MAPPER.valueToTree(body), context);
    } catch (IllegalArgumentException e) {
      // result not serializable: skipped
    }
  }

  private static void collectNested(JsonNode node, Map<String, Set<String>> context) {
    collectNested(node, context, null);
  }

  /**
   * @param validityDate the validity date of the closest enclosing (or same) node that has one, so
   *     that each nav is paired with the validity date that applies to it: on the payment position
   *     in V1, on the payment option in V3
   */
  private static void collectNested(
      JsonNode node, Map<String, Set<String>> context, String validityDate) {
    String currentValidity = validityDate;
    if (node.isObject()) {
      CTX_NESTED_FIELDS.forEach((field, key) -> addValue(context, key, node.get(field)));
      JsonNode validity = node.get(VALIDITY_DATE_FIELD);
      if (validity != null && validity.isTextual() && !validity.asText().isEmpty()) {
        currentValidity = validity.asText();
      }
      JsonNode nav = node.get(NAV_FIELD);
      if (currentValidity != null && nav != null && nav.isTextual() && !nav.asText().isEmpty()) {
        addNavValidity(context, nav.asText(), currentValidity);
      }
    }
    String validityForChildren = currentValidity;
    node.forEach(child -> collectNested(child, context, validityForChildren));
  }

  /** Keeps a single "nav=validityDate" entry per nav, the last collected one (the response). */
  private static void addNavValidity(
      Map<String, Set<String>> context, String nav, String validityDate) {
    Set<String> entries = context.computeIfAbsent(NAV_VALIDITY_KEY, k -> new LinkedHashSet<>());
    entries.removeIf(entry -> entry.startsWith(nav + "="));
    entries.add(nav + "=" + validityDate);
  }

  private String getApiVersion() {
    try {
      Matcher matcher = VERSION_PREFIX.matcher(httRequest.getRequestURI());
      return matcher.find() ? matcher.group(1) : DEFAULT_API_VERSION;
    } catch (IllegalStateException e) {
      return DEFAULT_API_VERSION;
    }
  }

  private static void addValue(Map<String, Set<String>> context, String key, JsonNode value) {
    if (key == null || value == null) {
      return;
    }
    if (value.isArray()) {
      value.forEach(item -> addValue(context, key, item));
    } else if (value.isValueNode() && !value.isNull() && !value.asText().isEmpty()) {
      context.computeIfAbsent(key, k -> new LinkedHashSet<>()).add(value.asText());
    }
  }

  private static void putContextInMdc(Map<String, Set<String>> context) {
    context.forEach(
        (key, values) ->
            MDC.put(key, values.stream().limit(MAX_LOGGED_VALUES).collect(Collectors.joining(","))));
  }

  @Pointcut("execution(* *..controller..api..*(..))")
  public void restController() {
    // all rest controllers
  }

  @Pointcut(
      "@within(org.springframework.stereotype.Repository) || "
          + "target(org.springframework.data.repository.Repository)")
  public void repository() {
    // all repository methods
  }

  @Pointcut("@within(org.springframework.stereotype.Service)")
  public void service() {
    // all service methods
  }

  /** Log essential info of application during the startup. */
  @PostConstruct
  public void logStartup() {
    log.info("-> Starting {} version {} - environment {}", name, version, environment);
  }

  @Around(value = "restController()")
  public Object logApiInvocation(ProceedingJoinPoint joinPoint) throws Throwable {
    MDC.put(METHOD, joinPoint.getSignature().getName());
    MDC.put(START_TIME, String.valueOf(System.currentTimeMillis()));
    MDC.put(OPERATION_ID, UUID.randomUUID().toString());
    if (MDC.get(REQUEST_ID) == null) {
      var requestId = UUID.randomUUID().toString();
      MDC.put(REQUEST_ID, requestId);
    }
    Map<String, Set<String>> context = new LinkedHashMap<>();
    context.put(API_VERSION_KEY, new LinkedHashSet<>(Set.of(getApiVersion())));
    collectContextFromArguments(joinPoint, context);
    putContextInMdc(context);

    Object result;
    try {
      result = joinPoint.proceed();
    } catch (Throwable t) {
      // left in the MDC so the log written by the ErrorHandler reports the failed call
      MDC.put(STATUS, "KO");
      MDC.put(RESPONSE_TIME, getExecutionTime());
      if (t instanceof AppException appException) {
        // title and message are the ones already returned to the client
        MDC.put(CODE, String.valueOf(appException.getHttpStatus().value()));
        MDC.put(FAULT_CODE, appException.getTitle());
        MDC.put(FAULT_DETAIL, appException.getMessage());
      }
      throw t;
    }

    collectContextFromResult(result, context);
    putContextInMdc(context);

    MDC.put(STATUS, "OK");
    MDC.put(CODE, String.valueOf(httpResponse.getStatus()));
    MDC.put(RESPONSE_TIME, getExecutionTime());
    log.info("Completed API operation");
    context.keySet().forEach(MDC::remove);
    MDC.remove(STATUS);
    MDC.remove(CODE);
    MDC.remove(RESPONSE_TIME);
    MDC.remove(START_TIME);
    return result;
  }

  @AfterReturning(value = "execution(* *..exception.ErrorHandler.*(..))", returning = "result")
  public void trowingApiInvocation(JoinPoint joinPoint, ResponseEntity<ProblemJson> result) {
    MDC.put(STATUS, "KO");
    MDC.put(CODE, Integer.toString(result.getStatusCode().value()));
    MDC.put(RESPONSE_TIME, getExecutionTime());
    MDC.put(FAULT_CODE, getTitle(result));
    MDC.put(FAULT_DETAIL, getDetail(result));
    log.info("Failed API operation {} - error: {}", MDC.get(METHOD), result);
    MDC.clear();
  }

  @Around(value = "service()")
  public Object logTrace(ProceedingJoinPoint joinPoint) throws Throwable {
    Map<String, String> params = getParams(joinPoint);
    log.debug("Call method {} - args: {}", joinPoint.getSignature().toShortString(), params);
    Object result = joinPoint.proceed();
    log.debug("Return method {} - result: {}", joinPoint.getSignature().toShortString(), result);
    return result;
  }

  /** Logs only failed repository calls. Successful calls are not logged. */
  @AfterThrowing(value = "repository()", throwing = "ex")
  public void logRepositoryFailure(JoinPoint joinPoint, Throwable ex) {
    String eventAction =
        "DB-"
            + joinPoint.getSignature().getDeclaringType().getSimpleName()
            + "-"
            + joinPoint.getSignature().getName();
    FailureLogger.log("Database operation failed", eventAction, "postgres", null, ex);
  }
}
