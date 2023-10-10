package it.gov.pagopa.debtposition.config.feign;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class NodeFeignConfig extends AuthFeignConfig {

  private static final String NODE_SUBKEY_PLACEHOLDER = "${node.subscription-key}";

  @Autowired
  public NodeFeignConfig(@Value(NODE_SUBKEY_PLACEHOLDER) String subscriptionKey) {
    this.subscriptionKey = subscriptionKey;
  }
}
