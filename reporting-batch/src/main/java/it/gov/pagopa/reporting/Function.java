package it.gov.pagopa.reporting;

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

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.TimerTrigger;

import it.gov.pagopa.reporting.service.FlowsService;
import it.gov.pagopa.reporting.service.NodoChiediElencoFlussi;

import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.TipoElencoFlussiRendicontazione;

/**
 * Azure Functions with Timer trigger.
 */
public class Function {

    private String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private String flowsTable = System.getenv("FLOWS_TABLE");
    private String flowsQueue = System.getenv("FLOWS_QUEUE");

    /**
     * This function will be invoked periodically according to the specified
     * schedule.
     */
    @FunctionName("ReportingBatchFunction")
    public void run(@TimerTrigger(name = "ReportingBatchTrigger", schedule = "0 */1 * * * *") String timerInfo,
            final ExecutionContext context) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "Reporting Batch Trigger function executed at: " + LocalDateTime.now());

        NodoChiediElencoFlussi nodeClient = this.getNodeClientInstance();
        FlowsService flowsService = this.getFlowsServiceInstance(logger);

        try {

            nodeClient.setSslContext();

            // call NODO dei pagamenti
            String idPa = "00595780131";
            nodeClient.nodoChiediElencoFlussiRendicontazione(idPa);

            // retrieve result
            FaultBean faultBean = nodeClient.getNodoChiediElencoFlussiRendicontazioneFault();

            TipoElencoFlussiRendicontazione elencoFlussi = nodeClient
                    .getNodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione();

            if (faultBean != null) {

                logger.log(Level.INFO, () -> "faultBean DESC " + faultBean.getDescription());
            } else if (elencoFlussi != null) {

                logger.log(Level.INFO, () -> "elencoFlussi TotRestituiti " + elencoFlussi.getTotRestituiti());
                flowsService.flowsProcessing(elencoFlussi.getIdRendicontazione(), idPa);
            }

        } catch (UnrecoverableKeyException | CertificateException | IOException | NoSuchAlgorithmException
                | KeyStoreException | InvalidKeySpecException | KeyManagementException e) {

            logger.log(Level.SEVERE, () -> "Configuration Error " + e.getLocalizedMessage());
        } catch (Exception e) {

            logger.log(Level.SEVERE, () -> "Generic Error " + e.getMessage());
        }

    }

    public NodoChiediElencoFlussi getNodeClientInstance() {

        return new NodoChiediElencoFlussi();
    }

    public FlowsService getFlowsServiceInstance(Logger logger) {

        return new FlowsService(this.storageConnectionString, this.flowsTable, this.flowsQueue, logger);
    }
}
