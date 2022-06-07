package it.gov.pagopa.payments.config;

import org.slf4j.MDC;
import org.springframework.context.annotation.Bean;

import feign.RequestInterceptor;

public class FeignConfig {

	static final String HEADER_REQUEST_ID = "X-Request-Id";

    @Bean
    public RequestInterceptor requestIdInterceptor() {
        return requestTemplate -> requestTemplate.header(HEADER_REQUEST_ID, MDC.get("requestId"));
    }
}
