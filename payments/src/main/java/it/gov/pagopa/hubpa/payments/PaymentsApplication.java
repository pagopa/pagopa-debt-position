package it.gov.pagopa.hubpa.payments;

import java.util.ArrayList;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.web.bind.annotation.RestController;

import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;

@SpringBootApplication
@Configuration
@ComponentScan(basePackages = { "it.gov.pagopa.hubpa" })
@PropertySource(value = "classpath:application.properties", ignoreResourceNotFound = true)
public class PaymentsApplication {

    public static void main(final String[] args) {
	SpringApplication.run(PaymentsApplication.class, args);
    }

    @Bean
    public Docket api() {
	return new Docket(DocumentationType.OAS_30).select()
		.apis(RequestHandlerSelectors.withClassAnnotation(RestController.class)).paths(PathSelectors.any())
		.build().apiInfo(apiInfo());

    }

    private ApiInfo apiInfo() {
	return new ApiInfo("Payments REST API",
		"Servizi rest di gestione Avvisi per l'applicazione TariTefa", "0.0.1", null, null, null,
		null, new ArrayList<>());

    }
}
