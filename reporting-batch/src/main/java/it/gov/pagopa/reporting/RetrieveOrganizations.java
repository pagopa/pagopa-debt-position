package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.TimerTrigger;
import it.gov.pagopa.reporting.service.FlowsService;
import it.gov.pagopa.reporting.service.NodoChiediElencoFlussi;
import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.TipoElencoFlussiRendicontazione;

import java.io.IOException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.security.spec.InvalidKeySpecException;
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
    @FunctionName("ReportingBatchFunction")
    public void run(@TimerTrigger(name = "ReportingBatchTrigger", schedule = "0 */1 * * * *") String timerInfo,
            final ExecutionContext context) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "Reporting Batch Trigger function executed at: " + LocalDateTime.now());

    }

}
