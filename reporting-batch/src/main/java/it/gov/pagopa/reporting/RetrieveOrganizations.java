package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.TimerTrigger;
import it.gov.pagopa.reporting.service.GPDService;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Timer trigger.
 */
public class RetrieveOrganizations {

    private String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private String flowsTable = System.getenv("FLOWS_TABLE");
    private String organizationsQueue = System.getenv("ORGANIZATIONS_QUEUE");
    private String gpdHost = System.getenv("GPD_HOST");

    /**
     * This function will be invoked periodically according to the specified
     * schedule.
     */
//    @TimerTrigger(name = "ReportingBatchTrigger", schedule = "0 */1 * * * *") String timerInfo,

    @FunctionName("ReportingBatchFunction")
    public void run(
            @TimerTrigger(name = "ReportingBatchTrigger", schedule = "* * * * *") String timerInfo,
            final ExecutionContext context
    ) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "Reporting Batch Trigger function executed at: " + LocalDateTime.now());

        // call GPD to retrieve organization list
        GPDService.getInstance().getOrganizations(LocalDate.now());

        // save organization list to flows table
        // retrieve organization list from flows table and add to organizations queue
    }

}
