package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.TimerTrigger;
import com.microsoft.azure.storage.StorageException;
import it.gov.pagopa.reporting.models.Organizations;
import it.gov.pagopa.reporting.service.GPDService;
import it.gov.pagopa.reporting.service.OrganizationsService;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Timer trigger.
 */
public class RetrieveOrganizations {

    private String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private String organizationsTable = System.getenv("ORGANIZATIONS_TABLE");
    private String organizationsQueue = System.getenv("ORGANIZATIONS_QUEUE");

    /**
     * This function will be invoked periodically according to the specified
     * schedule.
     */
//    @TimerTrigger(name = "ReportingBatchTrigger", schedule = "0 */1 * * * *") String timerInfo,

    @FunctionName("ReportingBatchFunction")
    public void run(
            @TimerTrigger(name = "ReportingBatchTrigger", schedule = "*/5 * * * * *") String timerInfo,
            final ExecutionContext context
    ) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "Reporting Batch Trigger function executed at: " + LocalDateTime.now());

        // call GPD to retrieve organization list
        Organizations organizationList = GPDService.getInstance().getOrganizations(LocalDate.now());

        // update organization list to flows table
        OrganizationsService organizationsService = new OrganizationsService(storageConnectionString, organizationsTable, organizationsQueue, logger);
        List<String> organizationListToProcess = organizationsService.processOrganizationList(organizationList);

        // add to organizations queue
        organizationsService.addToOrganizationsQueue(organizationListToProcess);

    }

}
