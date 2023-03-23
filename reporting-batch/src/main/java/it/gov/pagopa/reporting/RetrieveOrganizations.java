package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.TimerTrigger;
import it.gov.pagopa.reporting.service.OrganizationsService;

import java.time.LocalDateTime;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Timer trigger.
 */
public class RetrieveOrganizations {

    private final String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private final String organizationsTable = System.getenv("ORGANIZATIONS_TABLE");
    private final String organizationsQueue = System.getenv("ORGANIZATIONS_QUEUE");
    private final String timeToLiveInSeconds = System.getenv("QUEUE_RETENTION_SEC");
    private final String initialVisibilityDelayInSeconds = System.getenv("QUEUE_DELAY_SEC");

    /**
     * This function will be invoked periodically according to the specified
     * schedule.
     */
    //  schedule = "*/5 * * * * *"

    @FunctionName("ReportingBatchFunction")
    public void run(
            @TimerTrigger(name = "ReportingBatchTrigger", schedule = "%NCRON_SCHEDULE_BATCH%") String timerInfo,
            final ExecutionContext context
    ) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "Reporting Batch Trigger function executed at: " + LocalDateTime.now());

        // update organization list to flows table
        OrganizationsService organizationsService = this.getOrganizationsServiceInstance(logger);
        List<String> organizationListToProcess = organizationsService.getOrganizations();

        // add to organizations queue
        organizationsService.addToOrganizationsQueue(organizationListToProcess);

    }

    public OrganizationsService getOrganizationsServiceInstance(Logger logger) {
        return new OrganizationsService(this.storageConnectionString, this.organizationsTable, this.organizationsQueue, Integer.parseInt(timeToLiveInSeconds), Integer.parseInt(initialVisibilityDelayInSeconds), logger);
    }

}
