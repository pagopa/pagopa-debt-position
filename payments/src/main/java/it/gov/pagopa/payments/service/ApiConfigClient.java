package it.gov.pagopa.payments.service;

import org.slf4j.MDC;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

import feign.FeignException;
import feign.RequestInterceptor;
import it.gov.pagopa.payments.config.FeignConfig;
import it.gov.pagopa.payments.model.creditorinstitution.CreditorInstitutionDetails;


@FeignClient(value = "apiconfig", url = "${service.apiconfig.host}", configuration = FeignConfig.class)
public interface ApiConfigClient {

    @Cacheable(value = "payment-options")
    @Retryable(exclude = FeignException.FeignClientException.class, maxAttemptsExpression = "${retry.maxAttempts}",
            backoff = @Backoff(delayExpression = "${retry.maxDelay}"))
    @GetMapping(value = "/creditorinstitutions/{creditorinstitutioncode}")
    CreditorInstitutionDetails getOrganization(@PathVariable("creditorinstitutioncode") String creditorInstitutionCode);

}
