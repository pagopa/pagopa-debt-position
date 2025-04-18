package it.gov.pagopa.debtposition.config.feign;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class SendFeignConfig extends AuthFeignConfig {

  private static final String SEND_SUBKEY_PLACEHOLDER = "${send.subscription-key}";

  @Autowired
  public SendFeignConfig(@Value(SEND_SUBKEY_PLACEHOLDER) String subscriptionKey) {
    this.subscriptionKey = subscriptionKey;
  }
}
