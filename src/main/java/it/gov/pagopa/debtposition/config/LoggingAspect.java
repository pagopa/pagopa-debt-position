package it.gov.pagopa.debtposition.config;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import it.gov.pagopa.debtposition.config.logging.FailureLogger;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.model.ProblemJson;
import it.gov.pagopa.debtposition.util.CommonUtil;
import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
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

  private static final String ARGUMENT_PREFIX = "arguments.";

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

  /**
   * Puts the scalar call arguments in the MDC as {@code arguments.name} fields, so they are
   * searchable. Request bodies contribute their top-level scalar properties; nested objects, lists
   * and the excluded fields are skipped.
   */
  private static List<String> putArgumentsInMdc(ProceedingJoinPoint joinPoint) {
    List<String> keys = new ArrayList<>();
    String[] names = ((CodeSignature) joinPoint.getSignature()).getParameterNames();
    Object[] values = joinPoint.getArgs();
    for (int i = 0; i < names.length; i++) {
      try {
        JsonNode node = ARGS_MAPPER.valueToTree(values[i]);
        if (node.isObject()) {
          node.fields().forEachRemaining(e -> putArgument(keys, e.getKey(), e.getValue()));
        } else {
          putArgument(keys, names[i], node);
        }
      } catch (IllegalArgumentException e) {
        // argument not serializable: skipped
      }
    }
    return keys;
  }

  private static void putArgument(List<String> keys, String name, JsonNode value) {
    if (!value.isValueNode() || value.isNull() || value.asText().isEmpty()) {
      return;
    }
    if (EXCLUDED_ARGS.contains(name) || MDC.get(ARGUMENT_PREFIX + name) != null) {
      return;
    }
    MDC.put(ARGUMENT_PREFIX + name, value.asText());
    keys.add(ARGUMENT_PREFIX + name);
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
    List<String> argumentKeys = putArgumentsInMdc(joinPoint);

    Object result = joinPoint.proceed();

    MDC.put(STATUS, "OK");
    MDC.put(CODE, String.valueOf(httpResponse.getStatus()));
    MDC.put(RESPONSE_TIME, getExecutionTime());
    log.info("Completed API operation");
    argumentKeys.forEach(MDC::remove);
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
