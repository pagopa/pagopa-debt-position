package it.gov.pagopa.debtposition.config.logging;

import ch.qos.logback.classic.pattern.ThrowableProxyConverter;
import ch.qos.logback.classic.spi.ILoggingEvent;
import java.util.Arrays;

/**
 * Renders a stack trace reduced to its first two lines and its last line, so that logged errors do
 * not carry the entire trace.
 */
public class TrimmedThrowableConverter extends ThrowableProxyConverter {

  private static final int HEAD_LINES = 2;

  @Override
  public String convert(ILoggingEvent event) {
    String full = super.convert(event);
    if (full == null || full.isEmpty()) {
      return full;
    }
    String[] lines = Arrays.stream(full.split("\\R")).filter(l -> !l.isBlank()).toArray(String[]::new);
    if (lines.length <= HEAD_LINES + 1) {
      return String.join(System.lineSeparator(), lines);
    }
    return String.join(
        System.lineSeparator(), lines[0], lines[1], lines[lines.length - 1]);
  }
}
