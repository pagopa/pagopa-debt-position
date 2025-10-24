package it.gov.pagopa.debtposition;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.retry.annotation.EnableRetry;

@SpringBootApplication
@EnableFeignClients
@EnableRetry
public class DebtPositionApplication {

  public static void main(String[] args) {
    SpringApplication.run(DebtPositionApplication.class, args);
  }
}
