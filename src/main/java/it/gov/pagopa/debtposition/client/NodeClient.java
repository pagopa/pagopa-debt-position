package it.gov.pagopa.debtposition.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Retryable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import feign.FeignException;
import it.gov.pagopa.debtposition.config.feign.NodeFeignConfig;
import it.gov.pagopa.debtposition.model.checkposition.NodeCheckPositionModel;
import it.gov.pagopa.debtposition.model.checkposition.response.NodeCheckPositionResponse;

@FeignClient(value = "node", url = "${service.node.host}", configuration = NodeFeignConfig.class)
public interface NodeClient {

  @Retryable(
      exclude = FeignException.FeignClientException.class,
      maxAttemptsExpression = "${retry.maxAttempts}",
      backoff = @Backoff(delayExpression = "${retry.maxDelay}"))
  @PostMapping(
      value = "${service.check.position.path}",
      consumes = MediaType.APPLICATION_JSON_VALUE)
  NodeCheckPositionResponse getCheckPosition(@RequestBody NodeCheckPositionModel body);
}
