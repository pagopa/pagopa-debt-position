package it.gov.pagopa.payments;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;

@SpringBootApplication
//@Configuration
//@ComponentScan(basePackages = {"it.gov.pagopa"})
//@PropertySource(value = "classpath:application.properties", ignoreResourceNotFound = true)
@EnableFeignClients
public class PaymentsApplication {

    public static void main(final String[] args) {
        SpringApplication.run(PaymentsApplication.class, args);
    }

}
