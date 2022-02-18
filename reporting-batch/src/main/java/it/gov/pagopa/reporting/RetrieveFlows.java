package it.gov.pagopa.reporting;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.QueueTrigger;
import it.gov.pagopa.reporting.models.Organizations;
import it.gov.pagopa.reporting.service.FlowsService;
import it.gov.pagopa.reporting.service.NodoChiediElencoFlussi;
import it.gov.pagopa.reporting.servicewsdl.NodoChiediElencoFlussiRendicontazione;
import it.gov.pagopa.reporting.servicewsdl.NodoChiediElencoFlussiRendicontazioneRisposta;
import org.jboss.resteasy.client.jaxrs.ResteasyClient;
import org.jboss.resteasy.client.jaxrs.ResteasyClientBuilder;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.xml.ws.ResponseWrapper;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Azure Queue trigger.
 */
public class RetrieveFlows {

    /**
     * This function will be invoked when a new message is detected in the queue
     */
    @FunctionName("RetrieveFlows")
    public void run(
            @QueueTrigger(name = "RetrieveOrganizationsTrigger", queueName = "%ORGANIZATIONS_QUEUE%", connection = "FLOW_SA_CONNECTION_STRING") String message,
            final ExecutionContext context) {

        NodoChiediElencoFlussiRendicontazione req = new NodoChiediElencoFlussiRendicontazione();

        Logger logger = context.getLogger();

        try {

            logger.log(Level.INFO, () -> "[RetrieveOrganizationsTrigger START]  processed a message " + message);

            //Response response = ClientBuilder.newClient()
            Response response = new ResteasyClientBuilder().build()
              .target("http://localhost:8086/nodo-per-pa/v1"+"/nodoChiediElencoFlussiRendicontazione")
              .request()
              .property("SOAPAction", "nodoChiediElencoFlussiRendicontazione")
             .post(Entity.entity(req, MediaType.APPLICATION_XML));

            logger.log(Level.INFO, () -> "[RetrieveOrganizationsTrigger END]  processed a message " + message );
            NodoChiediElencoFlussiRendicontazioneRisposta res = response.readEntity(NodoChiediElencoFlussiRendicontazioneRisposta.class);
            logger.log(Level.INFO, () -> "[RetrieveOrganizationsTrigger END]  response " + res);

        } catch (Exception e) {

            logger.log(Level.SEVERE, () -> "[RetrieveOrganizationsTrigger Error] Generic Error " + e.getMessage() + " "
                    + e.getCause() + " - message " + message);
        }

    }

  public NodoChiediElencoFlussi getNodeClientInstance() {
    return new NodoChiediElencoFlussi();
  }
}
