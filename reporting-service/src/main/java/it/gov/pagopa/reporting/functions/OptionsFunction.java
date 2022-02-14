package it.gov.pagopa.hubpa.functions;

import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.QueueTrigger;
import it.gov.pagopa.hubpa.models.OptionsMessage;
import it.gov.pagopa.hubpa.service.OptionsService;

/**
 * Azure Functions with Azure Queue trigger.
 */
public class OptionsFunction {

    /**
     * This function will be invoked when a new message is detected in the queue
     */
    @FunctionName("OptionsFunction")
    public void run(
            @QueueTrigger(name = "OptionsMessageTrigger", queueName = "%OPTIONS_QUEUE%", connection = "FLOW_SA_CONNECTION_STRING") String message,
            final ExecutionContext context) {

        Logger logger = context.getLogger();

        try {

            logger.log(Level.INFO, () -> "[OptionsMessageTrigger START]  processed a message " + message);

            OptionsMessage options = new ObjectMapper().readValue(message, OptionsMessage.class);

            logger.log(Level.INFO, () -> "[OptionsMessageTrigger] Update flow " + options.getIdFlow() + " with data "
                    + options.getDateFlow());
            Arrays.stream(options.getIuvs())
                    .forEach(option -> logger.log(Level.INFO, () -> "Update to RENDICONTATO iuv : " + option));

            this.getOptionsServiceInstance(logger).callPaymentServiceToReportOption(options);

            logger.log(Level.INFO, () -> "[OptionsMessageTrigger END]  processed a message " + message);
        } catch (JsonProcessingException em) {

            logger.log(Level.SEVERE, () -> "[OptionsMessageTrigger Error] Invalid Message Queue " + em.getMessage()
                    + " - message " + message);
        } catch (Exception e) {

            logger.log(Level.SEVERE, () -> "[OptionsMessageTrigger Error] Generic Error " + e.getMessage() + " "
                    + e.getCause() + " - message " + message);
        }

    }

    public OptionsService getOptionsServiceInstance(Logger logger) {

        return new OptionsService(null, null, System.getenv("PAYMENTS_HOST"), System.getenv("AUX_DIGIT"), logger);
    }
}
