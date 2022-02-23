package it.gov.pagopa.reporting.functions;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.QueueTrigger;
import it.gov.pagopa.reporting.models.OptionsMessage;
import it.gov.pagopa.reporting.service.GPDService;
import it.gov.pagopa.reporting.service.OptionsService;

/**
 * Azure Functions with Azure Queue trigger.
 */
public class UpdateOption {

    /**
     * This function will be invoked when a new message is detected in the queue
     */
    @FunctionName("UpdateOptionFunction")
    public void run(
            @QueueTrigger(name = "UpdateOptionTrigger", queueName = "%OPTIONS_QUEUE%", connection = "FLOW_SA_CONNECTION_STRING") String message,
            final ExecutionContext context) {

        Logger logger = context.getLogger();

        try {

            logger.log(Level.INFO, () -> "[UpdateOptionFunction START]  processed a message " + message);

            OptionsMessage options = new ObjectMapper().readValue(message, OptionsMessage.class);

            logger.log(Level.INFO, () -> "[UpdateOptionFunction] Update flow " + options.getIdFlow() + " with data "
                    + options.getFlowDate());

            GPDService gpdService = this.getGPDServiceInstance();
            options.getPaymentOptions()
                    .forEach(paymentOption -> {
                        logger.log(Level.INFO, () -> "Update to RENDICONTATO iuv : " + paymentOption.getOptionId() + " - " + paymentOption.getTransferId());
                        gpdService.setReport(options.getIdPA(), paymentOption);
                    });

            logger.log(Level.INFO, () -> "[UpdateOptionFunction END]  processed a message " + message);
        } catch (JsonProcessingException em) {

            logger.log(Level.SEVERE, () -> "[UpdateOptionFunction Error] Invalid Message Queue " + em.getMessage()
                    + " - message " + message);
        } catch (Exception e) {

            logger.log(Level.SEVERE, () -> "[UpdateOptionFunction Error] Generic Error " + e.getMessage() + " "
                    + e.getCause() + " - message " + message);
        }

    }

    public GPDService getGPDServiceInstance() {
        return GPDService.getInstance();
    }
}
