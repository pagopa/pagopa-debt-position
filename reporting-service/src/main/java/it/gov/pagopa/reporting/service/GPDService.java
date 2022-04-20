package it.gov.pagopa.reporting.service;

import it.gov.pagopa.reporting.models.PaymentOption;
import it.gov.pagopa.reporting.models.RetryStep;
import lombok.SneakyThrows;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

public class GPDService {

    // /organizations/:idEC/paymentoptions/:IUV/transfers/{transferid}/report
    private static final String GPD_PAYMENT_OPTIONS_SERVICE = "/organizations/%s/paymentoptions/%s/transfers/%s/report";
    private static GPDService instance = null;
    private final String gpdHost = System.getenv("GPD_HOST");


    private GPDService() {
    }

    public static GPDService getInstance() {
        if (instance == null) {
            instance = new GPDService();
        }
        return instance;
    }

    /**
     * set report with retry
     *
     * @param idPA          fiscal code
     * @param paymentOption payment option
     * @param logger        for logging
     * @param invocationId
     * @return return true if the report is set correctly
     */
    @SneakyThrows
    public RetryStep setReport(String idPA, PaymentOption paymentOption, Logger logger, String invocationId) {
        var requestId = UUID.randomUUID().toString();

        logger.log(Level.INFO, () -> String.format(
                "[id=%s][requestId=%s][GPD CALL][setReport] RENDICONTATO iuv : %s , transfer: %s", invocationId, requestId, paymentOption.getOptionId(), paymentOption.getTransferId()));

        int status = callSetReport(idPA, paymentOption, requestId);
        if (status == 200) {
            return RetryStep.DONE;
        }
        logger.log(Level.WARNING, () -> String.format(
                "[id=%s][requestId=%s][GPD CALL][setReport] HTTP error status %s for iuv : %s , transfer: %s", invocationId, requestId, status, paymentOption.getOptionId(), paymentOption.getTransferId()));

        if (status >= 400 && status < 500) {
            // skip retry if the status is 4xx
            return RetryStep.ERROR;
        }

        return RetryStep.RETRY;
    }

    int callSetReport(String idPA, PaymentOption paymentOption, String requestId) {
        try {
            Response response = ClientBuilder.newClient()
                    .target(gpdHost + String.format(GPD_PAYMENT_OPTIONS_SERVICE, idPA, paymentOption.getOptionId(), paymentOption.getTransferId()))
                    .request()
                    .header("X-Request-ID", requestId)
                    .accept(MediaType.APPLICATION_JSON)
                    .post(Entity.text(""));
            return response.getStatus();
        } catch (Exception e) {
            return -1;
        }
    }
}
