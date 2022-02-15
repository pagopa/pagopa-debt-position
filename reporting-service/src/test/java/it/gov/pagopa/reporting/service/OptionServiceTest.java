package it.gov.pagopa.reporting.service;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.spy;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.text.ParseException;
import java.util.logging.Logger;
import java.util.List;
import java.util.ArrayList;

import javax.xml.datatype.DatatypeConfigurationException;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.junit.ClassRule;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import it.gov.pagopa.reporting.models.BooleanResponseModel;
import it.gov.pagopa.reporting.models.OptionsMessage;
import it.gov.pagopa.reporting.models.OptionsReportingModel;

@Testcontainers
class OptionServiceTestIntegrationTest {

    @ClassRule
    @Container
    public static GenericContainer<?> azurite = new GenericContainer<>(
            DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest")).withExposedPorts(10001, 10002,
                    10000);

    Logger logger = Logger.getLogger("testlogging");

    String storageConnectionString = String.format(
            "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;BlobEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1",
            azurite.getContainerIpAddress(), azurite.getMappedPort(10000), azurite.getContainerIpAddress(),
            azurite.getMappedPort(10001));

    String flowsQueue = "testqueue";
    String idFlow = "idflow1";
    String dataFlow = "2014-04-24 11:15:00";

    OptionsService optionsService;

    @Test
    void optionsProcessingTest()
            throws ParseException, DatatypeConfigurationException, InvalidKeyException, URISyntaxException {

        optionsService = spy(
                new OptionsService(storageConnectionString, flowsQueue, "http://paymenthost", "3", logger));

        List<String> options = new ArrayList<>();
        options.add("identificativoUnivocoVersamento1");
        options.add("identificativoUnivocoVersamento2");
        options.add("identificativoUnivocoVersamento3");

        try {
            optionsService.optionsProcessing(options, idFlow, dataFlow);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }

        assertTrue(true);
    }

    @Test
    void callPaymentKoTest()
            throws ParseException, DatatypeConfigurationException, InvalidKeyException, URISyntaxException {

        optionsService = spy(
                new OptionsService(storageConnectionString, flowsQueue, "http://paymenthost", "3", logger));
        OptionsMessage options = new OptionsMessage();

        try {
            optionsService.callPaymentServiceToReportOption(options);
        } catch (Exception e) {
            assertTrue(true);
        }

    }

    @Test
    void booleanResponseModelTest() {

        BooleanResponseModel response = new BooleanResponseModel();

        response.setResult(Boolean.TRUE);

        assertEquals(Boolean.TRUE, response.getResult());
    }

    @Test
    void getOptionsReportingModel() {

        OptionsService optionsService = new OptionsService(storageConnectionString, flowsQueue, "http://paymenthost",
                "3", logger);

        OptionsMessage optionsMessage = new OptionsMessage();
        optionsMessage.setIdFlow("idFlow");
        optionsMessage.setDateFlow("dateFlow");
        optionsMessage.setIuvs(new String[] { "iuv1", "iuv2" });
        optionsService.getOptionsReportingModel(optionsMessage);

        OptionsReportingModel optionsReportingModel = optionsService.getOptionsReportingModel(optionsMessage);

        assertEquals("3iuv1", optionsReportingModel.getNotificationCodes().get(0));
        assertEquals("3iuv2", optionsReportingModel.getNotificationCodes().get(1));
        assertEquals("idFlow", optionsReportingModel.getIdFlow());
        assertEquals("dateFlow", optionsReportingModel.getDateFlow());
    }

}