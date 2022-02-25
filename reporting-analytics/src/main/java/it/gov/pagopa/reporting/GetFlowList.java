package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.HttpMethod;
import com.microsoft.azure.functions.HttpRequestMessage;
import com.microsoft.azure.functions.annotation.BindingName;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.HttpTrigger;
import it.gov.pagopa.reporting.service.FlowsService;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Azure Queue trigger.
 */
public class GetFlowList {

    private String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private String flowsTable = System.getenv("FLOWS_TABLE");
    private String flowsQueue = System.getenv("FLOWS_QUEUE");
    /**
     * This function will be invoked when a new message is detected in the queue
     */
    @FunctionName("GetFlowList")
    public void run(
            @HttpTrigger(name = "GetFlowListTrigger",
                    methods = {HttpMethod.GET},
                    route = "organizations/{organizationId}/reportings"
            ) HttpRequestMessage<Optional<String>> request,
            @BindingName("organizationId") String organizationId,
            final ExecutionContext context) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "RetrieveFlows function executed at: " + LocalDateTime.now());

    }

  public FlowsService getFlowsServiceInstance(Logger logger) {
    return new FlowsService(this.storageConnectionString, this.flowsTable, this.flowsQueue, logger);
  }
}
