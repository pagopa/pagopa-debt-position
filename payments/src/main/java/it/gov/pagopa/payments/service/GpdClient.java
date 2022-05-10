package it.gov.pagopa.payments.service;

import feign.FeignException;
import feign.RequestInterceptor;
import it.gov.pagopa.payments.model.PaymentOptionModel;
import it.gov.pagopa.payments.model.PaymentOptionModelResponse;
import it.gov.pagopa.payments.model.PaymentsModelResponse;
import org.slf4j.MDC;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;


@FeignClient(value = "gpd", url = "${service.gpd.host}", configuration = GpdClient.FeignConfiguration.class)
public interface GpdClient {

    @Cacheable(value = "payment-options")
    @Retryable(exclude = FeignException.FeignClientException.class, maxAttemptsExpression = "${retry.maxAttempts}",
            backoff = @Backoff(delayExpression = "${retry.maxDelay}"))
    @GetMapping(value = "/organizations/{organizationfiscalcode}")
    String getOrganization(@PathVariable("organizationfiscalcode") String organizationFiscalCode);

    @Retryable(exclude = FeignException.FeignClientException.class, maxAttemptsExpression = "${retry.maxAttempts}",
            backoff = @Backoff(delayExpression = "${retry.maxDelay}"))
    @GetMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{iuv}")
    PaymentsModelResponse getPaymentOption(@PathVariable("organizationfiscalcode") String organizationFiscalCode,
                                           @PathVariable("iuv") String iuv);

    @Retryable(exclude = FeignException.FeignClientException.class, maxAttemptsExpression = "${retry.maxAttempts}",
            backoff = @Backoff(delayExpression = "${retry.maxDelay}"))
    @PostMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{iuv}/pay", consumes = MediaType.APPLICATION_JSON_VALUE)
    PaymentOptionModelResponse receiptPaymentOption(@PathVariable("organizationfiscalcode") String organizationFiscalCode,
                                                    @PathVariable("iuv") String iuv,
                                                    @RequestBody PaymentOptionModel body);

    @Configuration
    class FeignConfiguration {
        static final String HEADER_REQUEST_ID = "X-Request-Id";

        @Bean
        public RequestInterceptor requestIdInterceptor() {
            return requestTemplate -> requestTemplate.header(HEADER_REQUEST_ID, MDC.get("requestId"));
        }
    }

}
