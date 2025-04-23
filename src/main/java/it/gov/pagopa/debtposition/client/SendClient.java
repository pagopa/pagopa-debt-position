package it.gov.pagopa.debtposition.client;

import feign.FeignException;
import it.gov.pagopa.debtposition.config.feign.SendFeignConfig;
import it.gov.pagopa.debtposition.model.send.response.NotificationPriceResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = "send", url = "${service.send.host}", configuration = SendFeignConfig.class)
public interface SendClient {

  @Retryable(
      exclude = FeignException.FeignClientException.class,
      maxAttemptsExpression = "${retry.maxAttempts}", // todo config retry maxAttempts
      backoff = @Backoff(delayExpression = "${retry.maxDelay}")) // todo config retry maxAttempts
  @GetMapping(
      value = "${service.get.notification.fee.path}",
      consumes = MediaType.APPLICATION_JSON_VALUE)
  NotificationPriceResponse getNotificationFee(@PathVariable("paTaxId") String paFiscalCode, @PathVariable("noticeCode") String nav);
}
