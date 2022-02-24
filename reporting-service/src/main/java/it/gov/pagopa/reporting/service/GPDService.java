package it.gov.pagopa.reporting.service;

import it.gov.pagopa.reporting.models.PaymentOption;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

public class GPDService {

    private String gpdHost = System.getenv("GPD_HOST");
    // /organizations/:idEC/paymentoptions/:IUV/transfers/{transferid}/report
    private static final String gpdpaymentOptionsService = "/organizations/%s/paymentoptions/%s/transfers/%s/report";

    private static GPDService instance = null;

    private GPDService() {}

    public static GPDService getInstance() {
        if (instance == null) {
            instance = new GPDService();
        }
        return instance;
    }

    public boolean setReport(String idPA, PaymentOption paymentOption) {
        Response response = ClientBuilder.newClient()
                .target(gpdHost + String.format(gpdpaymentOptionsService, idPA, paymentOption.getOptionId(), paymentOption.getTransferId()))
                .request()
                .accept(MediaType.APPLICATION_JSON)
                .post(Entity.text(""));
        return response.getStatus() == 200;
    }
}
