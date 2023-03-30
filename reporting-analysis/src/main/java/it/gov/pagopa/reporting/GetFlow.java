package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.*;
import com.microsoft.azure.functions.annotation.AuthorizationLevel;
import com.microsoft.azure.functions.annotation.BindingName;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.HttpTrigger;
import it.gov.pagopa.reporting.service.FlowsService;

import javax.ws.rs.core.MediaType;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Azure Queue trigger.
 */
public class GetFlow {

    private String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");

    private String flowsTable = System.getenv("FLOWS_TABLE");

    private String containerBlob = System.getenv("FLOWS_CONTAINER");

    /**
     * This function will be invoked when a new message is detected in the queue
     * @return
     */
    @FunctionName("GetFlow")
    public HttpResponseMessage run (
            @HttpTrigger(name = "GetFlowTrigger",
                    methods = {HttpMethod.GET},
                    route = "organizations/{organizationId}/reportings/{flowId}/date/{flowDate}",
                    authLevel = AuthorizationLevel.ANONYMOUS
            ) HttpRequestMessage<Optional<String>> request,
            @BindingName("organizationId") String organizationId,
            @BindingName("flowId") String flowId,
            @BindingName("flowDate") String flowDate,
            final ExecutionContext context) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "RetrieveFlow function executed at: " + LocalDateTime.now());

        FlowsService flowsService = getFlowsServiceInstance(logger);

        try {
            String data = flowsService.getByFlow(organizationId, flowId, flowDate);

            return request.createResponseBuilder(HttpStatus.OK)
                    .header("Content-Type", MediaType.APPLICATION_XML)
                    .body(data)
                    .build();

        } catch (Exception e) {
            logger.log(Level.SEVERE, () -> "GetFlow error: " + e.getLocalizedMessage());

            return request.createResponseBuilder(HttpStatus.NOT_FOUND)
                    .header("Content-Type", "application/json")
                    .build();
        }
    }

    public FlowsService getFlowsServiceInstance(Logger logger) {
        return new FlowsService(this.storageConnectionString, this.flowsTable, this.containerBlob, logger);
    }

}
