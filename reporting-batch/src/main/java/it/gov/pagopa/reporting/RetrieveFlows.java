package it.gov.pagopa.reporting;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.QueueTrigger;
import com.sun.xml.ws.client.ClientTransportException;
import it.gov.pagopa.reporting.models.OrganizationsMessage;
import it.gov.pagopa.reporting.service.FlowsService;
import it.gov.pagopa.reporting.service.NodoChiediElencoFlussi;
import it.gov.pagopa.reporting.service.OrganizationsService;
import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.TipoElencoFlussiRendicontazione;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Azure Queue trigger.
 */
public class RetrieveFlows {

    private final String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private final String flowsTable = System.getenv("FLOWS_TABLE");
    private final String flowsQueue = System.getenv("FLOWS_QUEUE");
    private final String organizationsTable = System.getenv("ORGANIZATIONS_TABLE");
    private final String organizationsQueue = System.getenv("ORGANIZATIONS_QUEUE");
    private final String timeToLiveInSeconds = System.getenv("QUEUE_RETENTION_SEC");
    private final String initialVisibilityDelayInSeconds = System.getenv("QUEUE_DELAY_SEC");
    private final String maxRetryQueuing = System.getenv("MAX_RETRY_QUEUING");

    /**
     * This function will be invoked when a new message is detected in the queue
     */
    @FunctionName("RetrieveFlows")
    public void run(
            @QueueTrigger(name = "RetrieveOrganizationsTrigger", queueName = "%ORGANIZATIONS_QUEUE%", connection = "FLOW_SA_CONNECTION_STRING") String message,
            final ExecutionContext context) {

        Logger logger = context.getLogger();
        logger.log(Level.INFO, () -> String.format("[RetrieveOrganizationsTrigger START] processed the message: %s at %s", message, LocalDateTime.now()));

        NodoChiediElencoFlussi nodeClient = this.getNodeClientInstance(logger);
        FlowsService flowsService = this.getFlowsServiceInstance(logger);

        try {
            OrganizationsMessage organizationsMessage = new ObjectMapper().readValue(message, OrganizationsMessage.class);

            Arrays.stream(organizationsMessage.getIdPA())
                    .forEach((organization -> {
                        try {
                            // call NODO dei pagamenti
                            nodeClient.nodoChiediElencoFlussiRendicontazione(organization);

                            // retrieve result
                            FaultBean faultBean = nodeClient.getNodoChiediElencoFlussiRendicontazioneFault();

                            TipoElencoFlussiRendicontazione elencoFlussi = nodeClient.getNodoChiediElencoFlussiRendicontazione();

                            if (faultBean != null) {
                                logger.log(Level.WARNING, () -> "[RetrieveFlows] faultBean DESC " + faultBean.getDescription());
                            } else if (elencoFlussi != null) {
                                logger.log(Level.INFO, () -> "[RetrieveFlows] elencoFlussi PA " + organization + " TotRestituiti " + elencoFlussi.getTotRestituiti());
                                flowsService.flowsProcessing(elencoFlussi.getIdRendicontazione(), organization);
                            }
                        } catch (ClientTransportException e) {
                            logger.log(Level.SEVERE, () -> "[NODO Connection down] Organization: [" + organization +"] Caused by: " + e.getCause() + " Message: " + e.getMessage() + " Stack trace: " + Arrays.toString(e.getStackTrace()));
                            int retry = organizationsMessage.getRetry();
                            if (retry < Integer.parseInt(maxRetryQueuing)) {
                                OrganizationsService organizationsService = getOrganizationsServiceInstance(logger);
                                organizationsService.retryToOrganizationsQueue(organization, retry + 1);
                            } else {
                                logger.log(Level.SEVERE, () -> "[NODO Connection down]  Max retry exceeded.");
                            }
                        }

                    }));
        } catch (JsonProcessingException e) {
            logger.log(Level.SEVERE, () -> "[RetrieveOrganizationsTrigger]  Error " + e.getLocalizedMessage());
        }
    }

    public NodoChiediElencoFlussi getNodeClientInstance(Logger logger) {
        return new NodoChiediElencoFlussi(logger);
    }

    public FlowsService getFlowsServiceInstance(Logger logger) {
        return new FlowsService(this.storageConnectionString, this.flowsTable, this.flowsQueue, logger);
    }

    public OrganizationsService getOrganizationsServiceInstance(Logger logger) {
        return new OrganizationsService(this.storageConnectionString, this.organizationsTable, this.organizationsQueue, Integer.parseInt(timeToLiveInSeconds), Integer.parseInt(initialVisibilityDelayInSeconds), logger);
    }
}
