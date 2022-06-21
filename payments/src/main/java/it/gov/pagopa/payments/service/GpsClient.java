package it.gov.pagopa.payments.service;

import feign.FeignException;
import it.gov.pagopa.payments.config.FeignConfig;
import it.gov.pagopa.payments.model.spontaneous.PaymentPositionModel;
import it.gov.pagopa.payments.model.spontaneous.SpontaneousPaymentModel;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;


@FeignClient(value = "gps", url = "${service.gps.host}", configuration = FeignConfig.class)
public interface GpsClient {

    @Retryable(exclude = FeignException.FeignClientException.class, maxAttemptsExpression = "${retry.maxAttempts}",
            backoff = @Backoff(delayExpression = "${retry.maxDelay}"))
    @PostMapping(value = "/organizations/{organizationfiscalcode}/spontaneouspayments")
    PaymentPositionModel createSpontaneousPayments(@PathVariable("organizationfiscalcode") String organizationFiscalCode,
                                                   @RequestBody SpontaneousPaymentModel spontaneousPayment);


}
