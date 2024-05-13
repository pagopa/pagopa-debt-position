package it.gov.pagopa.debtposition.config;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.model.ProblemJson;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.*;
import org.aspectj.lang.reflect.CodeSignature;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.env.AbstractEnvironment;
import org.springframework.core.env.EnumerablePropertySource;
import org.springframework.core.env.Environment;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.stream.StreamSupport;


@Aspect
@Component
@Slf4j
public class LoggingAspect {
    public static final String START_TIME = "startTime";
    public static final String METHOD = "method";
    public static final String STATUS = "status";
    public static final String CODE = "httpCode";
    public static final String RESPONSE_TIME = "responseTime";
    public static final String RESPONSE = "response";
    public static final String FAULT_CODE = "faultCode";
    public static final String FAULT_DETAIL = "faultDetail";
    public static final String REQUEST_ID = "requestId";
    public static final String OPERATION_ID = "operationId";
    public static final String ARGS = "args";

    @Autowired
    HttpServletRequest httRequest;

    @Autowired
    HttpServletResponse httpResponse;

    @Value("${application.name}")
    private String name;

    @Value("${application.version}")
    private String version;

    @Value("${properties.environment}")
    private String environment;


    @Pointcut("@within(org.springframework.web.bind.annotation.RestController)")
    public void restController() {
        // all rest controllers
    }

    @Pointcut("@within(org.springframework.stereotype.Repository)")
    public void repository() {
        // all repository methods
    }

    @Pointcut("@within(org.springframework.stereotype.Service)")
    public void service() {
        // all service methods
    }

    @Pointcut("execution(* *..client.*(..))")
    public void client() {
        // all service methods
    }

    /**
     * Log essential info of application during the startup.
     */
    @PostConstruct
    public void logStartup() {
        log.info("-> Starting {} version {} - environment {}", name, version, environment);
    }

    /**
     * If DEBUG log-level is enabled prints the env variables and the application properties.
     *
     * @param event Context of application
     */
    @EventListener
    public void handleContextRefresh(ContextRefreshedEvent event) {
        final Environment env = event.getApplicationContext().getEnvironment();
        log.debug("Active profiles: {}", Arrays.toString(env.getActiveProfiles()));
        final MutablePropertySources sources = ((AbstractEnvironment) env).getPropertySources();
        StreamSupport.stream(sources.spliterator(), false)
                .filter(EnumerablePropertySource.class::isInstance)
                .map(ps -> ((EnumerablePropertySource<?>) ps).getPropertyNames())
                .flatMap(Arrays::stream)
                .distinct()
                .filter(prop -> !(prop.toLowerCase().contains("credentials") || prop.toLowerCase().contains("password") || prop.toLowerCase().contains("pass") || prop.toLowerCase().contains("pwd")))
                .forEach(prop -> log.debug("{}: {}", prop, env.getProperty(prop)));
    }

    @Around(value = "execution(* it.gov.pagopa.debtposition.repository..*.*(..)) || execution(* it.gov.pagopa.debtposition.service..*.*(..))")
    public Object logExecutionTime(ProceedingJoinPoint joinPoint) throws Throwable {
        long startTime = System.currentTimeMillis();
        Object result = joinPoint.proceed();
        long endTime = System.currentTimeMillis();
        log.debug("Time taken for Execution of {} is: {}ms", joinPoint.getSignature().toShortString(), (endTime - startTime));
        return result;
    }

    @Before(value = "execution(* it.gov.pagopa.debtposition.repository..*.*(..)) || execution(* it.gov.pagopa.debtposition.service..*.*(..))")
    public void logTrace(JoinPoint joinPoint) {
        log.trace("Trace method {} - args: {}", joinPoint.getSignature().toShortString(), joinPoint.getArgs());
    }

    @Around(value = "restController()")
    public Object logApiInvocation(ProceedingJoinPoint joinPoint) throws Throwable {
        MDC.put(METHOD, joinPoint.getSignature().getName());
        MDC.put(START_TIME, String.valueOf(System.currentTimeMillis()));
        MDC.put(OPERATION_ID, UUID.randomUUID().toString());
        if(MDC.get(REQUEST_ID) == null) {
            var requestId = UUID.randomUUID().toString();
            MDC.put(REQUEST_ID, requestId);
        }
        String params = getParams(joinPoint);
        MDC.put(ARGS, getParams(joinPoint));

        log.info("Invoking API operation {} - args: {}", joinPoint.getSignature().getName(), params);

        Object result = joinPoint.proceed();

        MDC.put(STATUS, "OK");
        MDC.put(CODE, String.valueOf(httpResponse.getStatus()));
        MDC.put(RESPONSE_TIME, getExecutionTime());
        MDC.put(RESPONSE, toJsonString(result));
        log.info("Successful API operation {} - result: {}", joinPoint.getSignature().getName(), result);
        MDC.remove(RESPONSE);
        MDC.remove(STATUS);
        MDC.remove(CODE);
        MDC.remove(RESPONSE_TIME);
        MDC.remove(START_TIME);
        return result;
    }

    @AfterReturning(value = "execution(* *..exception.ErrorHandler.*(..))", returning = "result")
    public void trowingApiInvocation(JoinPoint joinPoint, ResponseEntity<ProblemJson> result) {
        MDC.put(STATUS, "KO");
        MDC.put(CODE, String.valueOf(result.getStatusCodeValue()));
        MDC.put(RESPONSE_TIME, getExecutionTime());
        MDC.put(RESPONSE, toJsonString(result));
        MDC.put(FAULT_CODE, getTitle(result));
        MDC.put(FAULT_DETAIL, getDetail(result));
        log.info("Failed API operation {} - error: {}", MDC.get(METHOD), result);
        MDC.clear();
    }

    @Around(value = "repository() || service() || client()")
    public Object logTrace(ProceedingJoinPoint joinPoint) throws Throwable {
        String params = getParams(joinPoint);
        log.debug("Call method {} - args: {}", joinPoint.getSignature().toShortString(), params);
        Object result = joinPoint.proceed();
        log.debug("Return method {} - result: {}", joinPoint.getSignature().toShortString(), result);
        return result;
    }

    private static String getDetail(ResponseEntity<ProblemJson> result) {
        if(result != null && result.getBody() != null && result.getBody().getDetail() != null) {
            return result.getBody().getDetail();
        } else return AppError.UNKNOWN.getDetails();
    }

    private static String getTitle(ResponseEntity<ProblemJson> result) {
        if(result != null && result.getBody() != null && result.getBody().getTitle() != null) {
            return result.getBody().getTitle();
        } else return AppError.UNKNOWN.getTitle();
    }

    public static String getExecutionTime() {
        String startTime = MDC.get(START_TIME);
        if(startTime != null) {
            long endTime = System.currentTimeMillis();
            long executionTime = endTime - Long.parseLong(startTime);
            return String.valueOf(executionTime);
        }
        return "-";
    }

    private static String getParams(ProceedingJoinPoint joinPoint) {
        CodeSignature codeSignature = (CodeSignature) joinPoint.getSignature();
        Map<String, Object> params = new HashMap<>();
        int i = 0;
        for (var paramName : codeSignature.getParameterNames()) {
            Object param = joinPoint.getArgs()[i++];
            params.put(paramName, param);
        }
        return toJsonString(params);

    }

    private static String toJsonString(Object param) {
        try {
            return new ObjectMapper()
                    .registerModule(new JavaTimeModule())
                    .writeValueAsString(param);
        } catch (JsonProcessingException e) {
            log.warn("An error occurred when trying to parse a parameter", e);
            return "parsing error";
        }
    }
}