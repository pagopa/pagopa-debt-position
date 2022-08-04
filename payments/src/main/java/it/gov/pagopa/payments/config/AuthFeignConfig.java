package it.gov.pagopa.payments.config;

import feign.RequestInterceptor;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

@Component
public class AuthFeignConfig {

    @Value("${apiconfig.subscription-key}")
    private String subscriptionKey;

    static final String HEADER_REQUEST_ID = "X-Request-Id";
    static final String HEADER_SUBSCRIBTION_KEY = "Ocp-Apim-Subscription-Key";

    @Bean
    public RequestInterceptor requestIdInterceptor() {
        return requestTemplate -> requestTemplate
                .header(HEADER_REQUEST_ID, MDC.get("requestId"))
                .header(HEADER_SUBSCRIBTION_KEY, subscriptionKey);
    }
}
