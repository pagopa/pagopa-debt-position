package it.gov.pagopa.debtposition.util;

import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;

import java.util.Calendar;
import java.util.UUID;

import static it.gov.pagopa.debtposition.config.LoggingAspect.*;

@Slf4j
public class SchedulerUtils {

    public static void updateMDCForStartExecution(String method, String args) {
        MDC.put(METHOD, method);
        MDC.put(START_TIME, String.valueOf(Calendar.getInstance().getTimeInMillis()));
        MDC.put(REQUEST_ID, UUID.randomUUID().toString());
        MDC.put(OPERATION_ID, UUID.randomUUID().toString());
        MDC.put(ARGS, args);
    }

    public static void updateMDCForEndExecution() {
        MDC.put(STATUS, "OK");
        MDC.put(CODE, "201");
        MDC.put(RESPONSE_TIME, getExecutionTime());
        log.info("Scheduled job finished successfully");
    }

    public static void updateMDCError(Exception e, String method) {
        MDC.put(STATUS, "KO");
        MDC.put(CODE, "500");
        MDC.put(RESPONSE_TIME, getExecutionTime());
        MDC.put(FAULT_CODE, method);
        MDC.put(FAULT_DETAIL, e.getMessage());
        log.info("An error occurring during a scheduled job");
    }
}
