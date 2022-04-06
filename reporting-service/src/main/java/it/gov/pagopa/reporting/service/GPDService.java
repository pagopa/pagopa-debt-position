package it.gov.pagopa.reporting.service;

import it.gov.pagopa.reporting.models.PaymentOption;
import lombok.SneakyThrows;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Optional;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import static java.lang.Thread.sleep;

public class GPDService {

    private final String gpdHost = System.getenv("GPD_HOST");
    private final String maxAttemps = System.getenv("MAX_ATTEMPTS");
    private final String delay = System.getenv("DELAY_ATTEMPS");
    // /organizations/:idEC/paymentoptions/:IUV/transfers/{transferid}/report
    private static final String GPD_PAYMENT_OPTIONS_SERVICE = "/organizations/%s/paymentoptions/%s/transfers/%s/report";

    private static GPDService instance = null;

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
     * @return return true if the report is set correctly
     */
    @SneakyThrows
    public boolean setReport(String idPA, PaymentOption paymentOption, Logger logger) {
        int maxAttemptsValue = Integer.parseInt(Optional.ofNullable(maxAttemps)
                .orElse("1")); // for testing
        int delayValue = Integer.parseInt(Optional.ofNullable(delay)
                .orElse("0")); // for testing
        for (int i = 1; i <= maxAttemptsValue; i++) {
            var requestId = UUID.randomUUID().toString();

            logger.log(Level.INFO, () -> String.format(
                    "[requestId=%s][GPD CALL][setReport] RENDICONTATO iuv : %s , transfer: %s", requestId, paymentOption.getOptionId(), paymentOption.getTransferId()));

            int status = callSetReport(idPA, paymentOption, requestId);
            if (status == 200) {
                return true;
            }
            if (status >= 400 && status < 500) {
                // skip retry if the status is 4xx
                return false;
            }
            sleep(delayValue);
            logger.log(Level.WARNING, () -> String.format(
                    "[requestId=%s][GPD Error][setReport] Retry for RENDICONTATO iuv : %s , transfer: %s", requestId, paymentOption.getOptionId(), paymentOption.getTransferId()));
        }
        return false;
    }

    private int callSetReport(String idPA, PaymentOption paymentOption, String requestId) {
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
