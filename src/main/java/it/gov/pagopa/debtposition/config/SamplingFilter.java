package it.gov.pagopa.debtposition.config;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;
import java.util.concurrent.atomic.AtomicLong;
import lombok.Setter;

public class SamplingFilter extends Filter<ILoggingEvent> {

  // Setter for the sampling rate (configurable from XML)
  // The rate of sampling. '100' means 1 out of 100 INFO logs will be kept after the burst.
  @Setter private int rate = 100;

  // Setter for the burst size (configurable from XML)
  // The number of initial INFO logs to allow before sampling begins.
  @Setter private int maxBurst = 0; // Defaults to 0 for no burst.

  // A thread-safe counter for INFO logs
  private final AtomicLong counter = new AtomicLong(0);

  @Override
  public FilterReply decide(ILoggingEvent event) {
    // We only want to sample INFO level logs
    if (event.getLevel().equals(Level.INFO)) {
      long currentCount = counter.incrementAndGet();

      if (currentCount < 0) {
        counter.set(0); // Reset the master counter
        currentCount = counter.incrementAndGet();
      }

      // 1. Check if we are still within the initial burst limit.
      if (currentCount <= maxBurst) {
        return FilterReply.ACCEPT; // Always accept logs during the burst.
      }

      // 2. After the burst, apply the sampling logic.
      // Note: We use the same counter, so sampling continues naturally.
      if (currentCount % rate == 0) {
        return FilterReply.ACCEPT;
      } else {
        return FilterReply.DENY;
      }
    } else {
      // For any other level (ERROR, WARN, DEBUG), make no decision.
      return FilterReply.NEUTRAL;
    }
  }
}
