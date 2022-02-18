package it.gov.pagopa.reporting;

import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.FunctionName;
import com.microsoft.azure.functions.annotation.QueueTrigger;
import it.gov.pagopa.reporting.service.NodoChiediElencoFlussi;
import it.gov.pagopa.reporting.servicewsdl.NodoChiediElencoFlussiRendicontazione;
import it.gov.pagopa.reporting.servicewsdl.NodoChiediElencoFlussiRendicontazioneRisposta;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.ByteArrayInputStream;
import java.io.StringReader;
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
        req.setIdentificativoDominio("700000001");
        req.setIdentificativoIntermediarioPA("700000001");
        req.setIdentificativoPSP("700000001");
        req.setIdentificativoStazioneIntermediarioPA("700000001");
        req.setPassword("pwdpwdpwd");

        Logger logger = context.getLogger();

        try {

            logger.log(Level.INFO, () -> "[RetrieveOrganizationsTrigger START]  processed a message " + message);

//            Response response = ClientBuilder.newClient()
//              .target("http://localhost:8086/nodo-per-pa/v1"+"/nodoChiediElencoFlussiRendicontazione")
//              .request()
//              .property("SOAPAction", "nodoChiediElencoFlussiRendicontazione")
//             .post(Entity.entity(req, MediaType.APPLICATION_XML));

            String response = ClientBuilder.newClient()
                    .target("http://localhost:8086/nodo-per-pa/v1" + "/nodoChiediElencoFlussiRendicontazione")
                    .request()
                    .post(Entity.entity(req, MediaType.WILDCARD), String.class);

            logger.log(Level.INFO, () -> "[RetrieveOrganizationsTrigger END]  processed a message " + message );

            JAXBContext jaxbContext = JAXBContext.newInstance(NodoChiediElencoFlussiRendicontazioneRisposta.class);
            Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
            Object unmarshal = jaxbUnmarshaller.unmarshal(new ByteArrayInputStream(response.getBytes()));


//            XmlMapper xmlMapper = new XmlMapper();
//            NodoChiediElencoFlussiRendicontazioneRisposta nodoChiediElencoFlussiRendicontazioneRisposta =
//                    xmlMapper.readValue(response, NodoChiediElencoFlussiRendicontazioneRisposta.class);



//            NodoChiediElencoFlussiRendicontazioneRisposta res = response.readEntity(NodoChiediElencoFlussiRendicontazioneRisposta.class);
//            logger.log(Level.INFO, () -> "[RetrieveOrganizationsTrigger END]  response " + nodoChiediElencoFlussiRendicontazioneRisposta);

        } catch (Exception e) {

            logger.log(Level.SEVERE, () -> "[RetrieveOrganizationsTrigger Error] Generic Error " + e.getMessage() + " "
                    + e.getCause() + " - message " + message);
        }

    }

  public NodoChiediElencoFlussi getNodeClientInstance() {
    return new NodoChiediElencoFlussi();
  }
}
