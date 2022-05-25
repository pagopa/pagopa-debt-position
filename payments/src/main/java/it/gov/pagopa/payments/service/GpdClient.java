package it.gov.pagopa.payments.service;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import feign.FeignException;
import it.gov.pagopa.payments.config.FeignConfig;
import it.gov.pagopa.payments.model.PaymentOptionModel;
import it.gov.pagopa.payments.model.PaymentOptionModelResponse;
import it.gov.pagopa.payments.model.PaymentsModelResponse;


@FeignClient(value = "gpd", url = "${service.gpd.host}", configuration = FeignConfig.class)
public interface GpdClient {

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
}
