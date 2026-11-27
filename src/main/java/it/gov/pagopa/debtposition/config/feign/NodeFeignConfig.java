package it.gov.pagopa.debtposition.config.feign;

import feign.Capability;
import it.gov.pagopa.debtposition.config.logging.FeignLoggingCapability;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

@Component
public class NodeFeignConfig extends AuthFeignConfig {

  private static final String NODE_SUBKEY_PLACEHOLDER = "${node.subscription-key}";

  @Autowired
  public NodeFeignConfig(@Value(NODE_SUBKEY_PLACEHOLDER) String subscriptionKey) {
    this.subscriptionKey = subscriptionKey;
  }

  @Bean
  public Capability nodeFailureLoggingCapability() {
    return new FeignLoggingCapability("node");
  }
}
