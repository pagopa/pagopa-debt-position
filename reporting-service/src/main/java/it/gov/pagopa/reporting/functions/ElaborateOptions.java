package it.gov.pagopa.reporting.functions;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.BindingName;
import com.microsoft.azure.functions.annotation.BlobTrigger;
import com.microsoft.azure.functions.annotation.FunctionName;
import it.gov.pagopa.reporting.service.FlowXmlParser;
import it.gov.pagopa.reporting.service.OptionsService;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Azure Functions with Azure Blob trigger.
 */
public class ElaborateOptions {
    private final String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private final String optionsQueue = System.getenv("OPTIONS_QUEUE");

    /**
     * This function will be invoked when a new or updated blob is detected at the
     * specified path. The blob contents are provided as input to this function.
     */
    @FunctionName("ElaborateOptionsFunction")
    public void run(
            @BlobTrigger(name = "BlobXmlTrigger", path = "%FLOWS_XML_BLOB%/{name}", dataType = "binary", connection = "FLOW_SA_CONNECTION_STRING") byte[] content,
            @BindingName("name") String name, final ExecutionContext context) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "Blob Trigger function executed at: " + LocalDateTime.now() + " for blob " + name);

        // XML File
        String converted = new String(content, StandardCharsets.UTF_8);

        logger.log(Level.INFO, () -> converted);

        try {
            SAXParserFactory factory = SAXParserFactory.newInstance();
            // to be compliant, completely disable DOCTYPE declaration:
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);

            SAXParser saxParser = factory.newSAXParser();

            FlowXmlParser handler = new FlowXmlParser();
            saxParser.parse(new InputSource(new StringReader(converted)), handler);

            OptionsService optionsService = this.getOptionsServiceInstance(logger);

            // dataOra##idPa##idflow.xml
            String[] flowInfo = name.split("##");
            String dataFlow = flowInfo[0];
            String idPA = flowInfo[1];
            String idFlow = flowInfo[2].substring(0, flowInfo[2].length() - 4); // remove extension file;
            logger.log(Level.INFO, () -> "Processing flow " + idPA + "/" + idFlow + " with date " + dataFlow);

            // step 11
            optionsService.optionsProcessing(handler.getOptions(), idPA, idFlow, dataFlow);

        } catch (ParserConfigurationException | SAXException | IOException e) {
            logger.log(Level.INFO, () -> "Processing flow exception: " + e.getMessage());
        }
    }

    public OptionsService getOptionsServiceInstance(Logger logger) {
        return new OptionsService(this.storageConnectionString, this.optionsQueue, logger);
    }

}
