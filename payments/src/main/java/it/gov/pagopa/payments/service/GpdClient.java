package it.gov.pagopa.payments.service;

import it.gov.pagopa.payments.model.PaymentsModelResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;


@FeignClient(value = "gpd", url = "${service.gpd.host}")
public interface GpdClient {


    @GetMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{iuv}")
    PaymentsModelResponse getPaymentOption(@PathVariable("organizationfiscalcode") String organizationFiscalCode,
                                           @PathVariable("iuv") String iuv);


}
