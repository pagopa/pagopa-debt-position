package it.gov.pagopa.debtposition.config.feign;

import feign.RequestInterceptor;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

@Component
public class SendFeignConfig {
  static final String HEADER_REQUEST_ID = "X-Request-Id";
  static final String HEADER_SUBSCRIBTION_KEY = "x-api-key";
  private static final String SEND_SUBKEY_PLACEHOLDER = "${send.subscription-key}";
  private final String subscriptionKey;

  @Autowired
  public SendFeignConfig(@Value(SEND_SUBKEY_PLACEHOLDER) String subscriptionKey) {
    this.subscriptionKey = subscriptionKey;
  }

  @Bean
  public RequestInterceptor requestInterceptor() {
    return requestTemplate ->
            requestTemplate
                    .header(HEADER_REQUEST_ID, MDC.get("requestId"))
                    .header(HEADER_SUBSCRIBTION_KEY, subscriptionKey);
  }
}
