package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.QueueTrigger;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Azure Queue trigger.
 */
public class RetrieveFlows {

    /**
     * This function will be invoked when a new message is detected in the queue
     */
//    @FunctionName("RetrieveFlows")
//    public void run(
//            @QueueTrigger(name = "RetrieveOrganizationsTrigger", queueName = "%ORGANIZATIONS_QUEUE%", connection = "FLOW_SA_CONNECTION_STRING") String message,
//            final ExecutionContext context) {
//
//        Logger logger = context.getLogger();
//
//        try {
//
//            logger.log(Level.INFO, () -> "[RetrieveOrganizationsTrigger START]  processed a message " + message);
//
//            logger.log(Level.INFO, () -> "[RetrieveOrganizationsTrigger END]  processed a message " + message);
//        } catch (Exception e) {
//
//            logger.log(Level.SEVERE, () -> "[RetrieveOrganizationsTrigger Error] Generic Error " + e.getMessage() + " "
//                    + e.getCause() + " - message " + message);
//        }
//
//    }

}
