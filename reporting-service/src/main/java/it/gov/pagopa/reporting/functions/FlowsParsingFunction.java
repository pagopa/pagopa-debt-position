package it.gov.pagopa.hubpa.functions;

import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.functions.annotation.BindingName;
import com.microsoft.azure.functions.annotation.BlobTrigger;
import com.microsoft.azure.functions.annotation.FunctionName;
import it.gov.pagopa.hubpa.service.FlowXmlParser;
import it.gov.pagopa.hubpa.service.OptionsService;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

/**
 * Azure Functions with Azure Blob trigger.
 */
public class FlowsParsingFunction {
    private String storageConnectionString = System.getenv("FLOW_SA_CONNECTION_STRING");
    private String optionsQueue = System.getenv("OPTIONS_QUEUE");

    /**
     * This function will be invoked when a new or updated blob is detected at the
     * specified path. The blob contents are provided as input to this function.
     */
    @FunctionName("FlowsParsingFunction")
    public void run(
            @BlobTrigger(name = "BlobXmlTrigger", path = "%FLOWS_XML_BLOB%/{name}", dataType = "binary", connection = "FLOW_SA_CONNECTION_STRING") byte[] content,
            @BindingName("name") String name, final ExecutionContext context) {

        Logger logger = context.getLogger();

        logger.log(Level.INFO, () -> "Blob Trigger function executed at: " + LocalDateTime.now() + " for blob " + name);

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

            String[] flowInfo = name.split("##");
            String idFlow = flowInfo[0];
            String dataFlow = flowInfo[1].substring(0,flowInfo[1].length()-4); // remove extension file
            logger.log(Level.INFO, () -> "Processing flow " + idFlow + " with date " + dataFlow);
            optionsService.optionsProcessing(handler.getOptions(), idFlow, dataFlow);

        } catch (ParserConfigurationException | SAXException | IOException e) {
            logger.log(Level.INFO, () -> "Processing flow exception: " + e.getMessage());
        }
    }

    public OptionsService getOptionsServiceInstance(Logger logger) {

        return new OptionsService(this.storageConnectionString, this.optionsQueue, System.getenv("PAYMENTS_HOST"), null, logger);
    }

}
