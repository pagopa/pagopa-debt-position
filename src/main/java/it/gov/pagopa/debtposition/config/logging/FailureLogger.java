package it.gov.pagopa.debtposition.config.logging;

import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;

/** Emits a single ERROR line for a failed I/O call, with the context keys needed to correlate it. */
@Slf4j
public final class FailureLogger {

  public static final String EVENT_ACTION = "event_action";
  public static final String EVENT_OUTCOME = "event_outcome";
  public static final String DEPENDENCY = "dependency";
  public static final String HTTP_CODE = "httpCode";
  public static final String OUTCOME_FAILURE = "failure";
  public static final String ERROR_TYPE = "error.type";
  public static final String ERROR_MESSAGE = "error.message";

  private static final int MAX_MESSAGE_LENGTH = 200;

  private FailureLogger() {}

  /**
   * Logs a failure at ERROR level. The MDC keys are removed afterwards, so they do not leak into
   * subsequent log lines of the same thread.
   *
   * @param message fixed message describing the failed operation
   * @param eventAction logical action, e.g. {@code DB-PaymentOptionRepository-findById}
   * @param dependency logical dependency name, e.g. {@code postgres} or {@code node}
   * @param httpCode HTTP status code of the outbound response, {@code null} when not applicable
   * @param cause exception the failure depends on, {@code null} when there is none. Only its type
   *     and a sanitized message are logged, never the stack trace
   */
  public static void log(
      String message, String eventAction, String dependency, Integer httpCode, Throwable cause) {
    MDC.put(EVENT_ACTION, eventAction);
    MDC.put(EVENT_OUTCOME, OUTCOME_FAILURE);
    MDC.put(DEPENDENCY, dependency);
    if (httpCode != null) {
      MDC.put(HTTP_CODE, String.valueOf(httpCode));
    }
    if (cause != null) {
      MDC.put(ERROR_TYPE, cause.getClass().getName());
      MDC.put(ERROR_MESSAGE, sanitize(cause.getMessage()));
    }
    try {
      log.error(message);
    } finally {
      MDC.remove(ERROR_TYPE);
      MDC.remove(ERROR_MESSAGE);
      MDC.remove(EVENT_ACTION);
      MDC.remove(EVENT_OUTCOME);
      MDC.remove(DEPENDENCY);
      MDC.remove(HTTP_CODE);
    }
  }

  /**
   * Keeps the first line only, masks quoted literals and key/value pairs (such as the ones in
   * constraint violation details) and truncates the result.
   */
  static String sanitize(String message) {
    if (message == null) {
      return "";
    }
    String firstLine = message.lines().findFirst().orElse("");
    String masked =
        firstLine
            .replaceAll("\\([^)]*\\)=\\([^)]*\\)", "(?)=(?)")
            .replaceAll("'[^']*'", "'?'")
            .replaceAll("\"[^\"]*\"", "\"?\"");
    return masked.length() > MAX_MESSAGE_LENGTH
        ? masked.substring(0, MAX_MESSAGE_LENGTH)
        : masked;
  }
}
