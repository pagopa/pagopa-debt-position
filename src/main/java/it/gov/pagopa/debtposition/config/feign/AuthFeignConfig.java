package it.gov.pagopa.debtposition.config.feign;

import feign.RequestInterceptor;
import org.slf4j.MDC;
import org.springframework.context.annotation.Bean;

public abstract class AuthFeignConfig {

  protected String subscriptionKey;

  static final String HEADER_REQUEST_ID = "X-Request-Id";

  static final String HEADER_SUBSCRIBTION_KEY = "Ocp-Apim-Subscription-Key";

  @Bean
  public RequestInterceptor requestIdInterceptor() {
    return requestTemplate ->
        requestTemplate
            .header(HEADER_REQUEST_ID, MDC.get("requestId"))
            .header(HEADER_SUBSCRIBTION_KEY, subscriptionKey);
  }
}
