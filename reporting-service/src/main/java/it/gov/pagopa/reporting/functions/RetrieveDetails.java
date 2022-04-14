package it.gov.pagopa.reporting.functions;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.QueueTrigger;
import it.gov.pagopa.reporting.models.FlowsMessage;
import it.gov.pagopa.reporting.service.FlowsService;

import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * FlowsXmlDownloadFunction Azure Functions with Azure Queue trigger.
 */
public class RetrieveDetails {
    /**
     * This function will be invoked when a new message is detected in the queue
     * FLOWS_QUEUE related to FLOW_SA_CONNECTION_STRING (app settings)
     */
    @FunctionName("RetrieveDetailsFunction")
    public void run(
            @QueueTrigger(name = "RetrieveDetailsTrigger", queueName = "%FLOWS_QUEUE%", connection = "FLOW_SA_CONNECTION_STRING") String message,
            final ExecutionContext context) {

        Logger logger = context.getLogger();

        try {

            logger.log(Level.INFO, () -> "[FlowsDownloadFunction START]  processed a message " + message);

            FlowsMessage flows = new ObjectMapper().readValue(message, FlowsMessage.class);

            // retrieve fdr from node
            this.getFlowsServiceInstance(logger)
                    .flowsXmlDownloading(Arrays.asList(flows.getFlows()), flows.getIdPA(), flows.getRetry() + 1);

            logger.log(Level.INFO, () -> "[FlowsDownloadFunction END]  processed a message " + message);
        } catch (JsonProcessingException em) {

            logger.log(Level.SEVERE, () -> "[FlowsDownloadFunction Error] Invalid Message Queue " + em.getMessage()
                    + " - message " + message);
        } catch (Exception e) {

            logger.log(Level.SEVERE, () -> "[FlowsDownloadFunction Error] Generic Error " + e.getMessage() + " "
                    + e.getCause() + " - message " + message);
        }
    }

    public FlowsService getFlowsServiceInstance(Logger logger) {
        String maxRetryQueuing = getVars("MAX_RETRY_QUEUING");
        String queueRetentionSec = getVars("QUEUE_RETENTION_SEC");
        String queueDelaySec = getVars("QUEUE_DELAY_SEC");
        return new FlowsService(System.getenv("FLOW_SA_CONNECTION_STRING"), System.getenv("PAA_ID_INTERMEDIARIO"),
                System.getenv("PAA_STAZIONE_INT"), System.getenv("PAA_PASSWORD"), System.getenv("FLOWS_XML_BLOB"), System.getenv("FLOWS_QUEUE"),
                System.getenv("FLOWS_TABLE"), Integer.parseInt(maxRetryQueuing), Integer.parseInt(queueRetentionSec), Integer.parseInt(queueDelaySec),
                logger);
    }

    public String getVars(String vars) {
        return System.getenv(vars);
    }
}
