package it.gov.pagopa.payments.service;

import it.gov.pagopa.payments.model.PaymentOptionModel;
import it.gov.pagopa.payments.model.PaymentsModelResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;


@FeignClient(value = "gpd", url = "${service.gpd.host}")
public interface GpdClient {

    @GetMapping(value = "/organizations/{organizationfiscalcode}")
    String getOrganization(@PathVariable("organizationfiscalcode") String organizationFiscalCode);

    @GetMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{iuv}")
    PaymentsModelResponse getPaymentOption(@PathVariable("organizationfiscalcode") String organizationFiscalCode,
                                           @PathVariable("iuv") String iuv);

    @PostMapping(value = "/organizations/{organizationfiscalcode}/paymentoptions/{iuv}/pay", consumes = MediaType.APPLICATION_JSON_VALUE)
    PaymentsModelResponse receiptPaymentOption(@PathVariable("organizationfiscalcode") String organizationFiscalCode,
                                               @PathVariable("iuv") String iuv,
                                               @RequestBody PaymentOptionModel body);


}
