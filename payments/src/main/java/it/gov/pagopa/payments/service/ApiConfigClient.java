package it.gov.pagopa.payments.service;

import feign.FeignException;
import it.gov.pagopa.payments.config.AuthFeignConfig;
import it.gov.pagopa.payments.model.creditorinstitution.StationCreditorInstitution;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;


@FeignClient(value = "apiconfig", url = "${service.apiconfig.host}", configuration = AuthFeignConfig.class)
public interface ApiConfigClient {

    @Cacheable(value = "get-organization")
    @Retryable(exclude = FeignException.FeignClientException.class, maxAttemptsExpression = "${retry.maxAttempts}",
            backoff = @Backoff(delayExpression = "${retry.maxDelay}"))
    @GetMapping(value = "/stations/{stationcode}/creditorinstitutions/{creditorinstitutioncode}")
    StationCreditorInstitution getOrganization(@PathVariable("stationcode") String stationCode, @PathVariable("creditorinstitutioncode") String creditorInstitutionCode);

}
