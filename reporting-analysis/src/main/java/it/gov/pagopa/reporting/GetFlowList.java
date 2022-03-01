package it.gov.pagopa.reporting;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.azure.functions.*;
import com.microsoft.azure.functions.annotation.BindingName;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.HttpTrigger;
import com.microsoft.azure.storage.StorageException;
import it.gov.pagopa.reporting.model.Flow;
import it.gov.pagopa.reporting.service.FlowsService;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Azure Queue trigger.
 */
public class GetFlowList {

    private String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");

    private String flowsTable = System.getenv("FLOWS_TABLE");

    private String containerBlob = System.getenv("FLOWS_XML_BLOB");

    /**
     * This function will be invoked when a new message is detected in the queue
     * @return
     */
    @FunctionName("GetFlowList")
    public HttpResponseMessage run (
            @HttpTrigger(name = "GetFlowListTrigger",
                    methods = {HttpMethod.GET},
                    route = "organizations/{organizationId}/reportings"
            ) HttpRequestMessage<Optional<String>> request,
            @BindingName("organizationId") String organizationId,
            final ExecutionContext context) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "RetrieveFlows function executed at: " + LocalDateTime.now());

        FlowsService flowsService = getFlowsServiceInstance(logger);

        try {
            List<Flow> flows = flowsService.getByOrganization(organizationId);
            ObjectMapper objectMapper = new ObjectMapper();
            final String data = objectMapper.writeValueAsString(flows);

            return request.createResponseBuilder(HttpStatus.OK)
                    .header("Content-Type", "application/json")
                    .body(data)
                    .build();

        } catch (URISyntaxException | InvalidKeyException | StorageException | JsonProcessingException e) {
            logger.log(Level.SEVERE, () -> "GetFlowList error: " + e.getLocalizedMessage());

            return request.createResponseBuilder(HttpStatus.BAD_REQUEST)
                    .header("Content-Type", "application/json")
                    .build();
        }
    }

    public FlowsService getFlowsServiceInstance(Logger logger) {
        return new FlowsService(this.storageConnectionString, this.flowsTable, this.containerBlob, logger);
    }

}
