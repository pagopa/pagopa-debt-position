package it.gov.pagopa.reporting;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.QueueTrigger;
import it.gov.pagopa.reporting.models.OrganizationsMessage;
import it.gov.pagopa.reporting.service.FlowsService;
import it.gov.pagopa.reporting.service.NodoChiediElencoFlussi;
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

    private String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private String flowsTable = System.getenv("FLOWS_TABLE");
    private String flowsQueue = System.getenv("FLOWS_QUEUE");
    /**
     * This function will be invoked when a new message is detected in the queue
     */
    @FunctionName("RetrieveFlows")
    public void run(
            @QueueTrigger(name = "RetrieveOrganizationsTrigger", queueName = "%ORGANIZATIONS_QUEUE%", connection = "FLOW_SA_CONNECTION_STRING") String message,
            final ExecutionContext context) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "RetrieveFlows function executed at: " + LocalDateTime.now());

        NodoChiediElencoFlussi nodeClient = this.getNodeClientInstance();
        FlowsService flowsService = this.getFlowsServiceInstance(logger);


            logger.log(Level.INFO, () -> "[RetrieveOrganizationsTrigger START]  processed a message " + message);

        OrganizationsMessage organizationsMessage = null;
        try {
            organizationsMessage = new ObjectMapper().readValue(message, OrganizationsMessage.class);

            Arrays.stream(organizationsMessage.getIdPA())
                    .forEach((organization -> {
                        logger.log(Level.INFO, () -> "call nodoChiediElencoFlussiRendicontazione for EC : " + organization);

                        // call NODO dei pagamenti
                        nodeClient.nodoChiediElencoFlussiRendicontazione(organization);

                        // retrieve result
                        FaultBean faultBean = nodeClient.getNodoChiediElencoFlussiRendicontazioneFault();

                        TipoElencoFlussiRendicontazione elencoFlussi = nodeClient
                                .getNodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione();

                        if (faultBean != null) {
                            logger.log(Level.INFO, () -> "faultBean DESC " + faultBean.getDescription());
                        } else if (elencoFlussi != null) {
                            logger.log(Level.INFO, () -> "elencoFlussi PA " + organization + " TotRestituiti " + elencoFlussi.getTotRestituiti() );
                            flowsService.flowsProcessing(elencoFlussi.getIdRendicontazione(), organization);

                        }

                    }));

        } catch (JsonProcessingException e) {
            this.logger.log(Level.SEVERE, () -> "[RetrieveOrganizationsTrigger]  Error " + e.getLocalizedMessage());
        }

    }

  public NodoChiediElencoFlussi getNodeClientInstance() {
    return new NodoChiediElencoFlussi();
  }
  public FlowsService getFlowsServiceInstance(Logger logger) {
    return new FlowsService(this.storageConnectionString, this.flowsTable, this.flowsQueue, logger);
    }

}
