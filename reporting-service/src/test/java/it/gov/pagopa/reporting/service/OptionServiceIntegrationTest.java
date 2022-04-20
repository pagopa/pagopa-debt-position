package it.gov.pagopa.reporting.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueueMessage;
import it.gov.pagopa.reporting.models.BooleanResponseModel;
import it.gov.pagopa.reporting.models.OptionsMessage;
import it.gov.pagopa.reporting.models.PaymentOption;
import it.gov.pagopa.reporting.models.RetryStep;
import org.junit.ClassRule;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import javax.xml.datatype.DatatypeConfigurationException;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.text.ParseException;
import java.util.List;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.spy;

@Testcontainers
class OptionServiceIntegrationTest {

    @ClassRule
    @Container
    public static GenericContainer<?> azurite = new GenericContainer<>(
            DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest"))
            .withExposedPorts(10001, 10002, 10000);

    Logger logger = Logger.getLogger("testlogging");

    String storageConnectionString = String.format(
            "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;BlobEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1",
            azurite.getContainerIpAddress(), azurite.getMappedPort(10000), azurite.getContainerIpAddress(),
            azurite.getMappedPort(10001));

    String flowsQueue = "testqueue";
    String idPA = "idPA";
    String idFlow = "idflow1";
    String dataFlow = "2014-04-24 11:15:00";

    OptionsService optionsService;

    @Test
    void optionsProcessingTest()
            throws InvalidKeyException, URISyntaxException, StorageException {

        optionsService = spy(new OptionsService(storageConnectionString, flowsQueue, logger));

        PaymentOption p1 = new PaymentOption("op1", 1, RetryStep.NONE.name());
        PaymentOption p2 = new PaymentOption("op2", 2, RetryStep.NONE.name());
        PaymentOption p3 = new PaymentOption("op3", 3, RetryStep.NONE.name());

        try {
            optionsService.optionsProcessing(List.of(p1, p2, p3), idPA, idFlow, dataFlow);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }

        Iterable<CloudQueueMessage> messagges = CloudStorageAccount.parse(storageConnectionString)
                .createCloudQueueClient().getQueueReference(this.flowsQueue).retrieveMessages(32);

        for (CloudQueueMessage cloudQueueMessage : messagges) {

            assertTrue(cloudQueueMessage.getMessageContentAsString().contains(p1.getOptionId())
                    || cloudQueueMessage.getMessageContentAsString().contains(p2.getOptionId())
                    || cloudQueueMessage.getMessageContentAsString().contains(p3.getOptionId()));
        }
    }


    @Test
    void insertMessageTest() throws URISyntaxException, InvalidKeyException, StorageException {

        optionsService = spy(new OptionsService(storageConnectionString, flowsQueue, logger));

        PaymentOption p1 = new PaymentOption("op1", 1, RetryStep.NONE.name());
        PaymentOption p2 = new PaymentOption("op2", 2, RetryStep.NONE.name());
        PaymentOption p3 = new PaymentOption("op3", 3, RetryStep.NONE.name());
        OptionsMessage message = new OptionsMessage();
        message.setIdPA(idPA);
        message.setIdFlow(idFlow);
        message.setRetryCount(0);
        message.setFlowDate(dataFlow);
        message.setPaymentOptions(List.of(p1, p2, p3));
        optionsService.insertMessage(message);

        Iterable<CloudQueueMessage> messagges = CloudStorageAccount.parse(storageConnectionString)
                .createCloudQueueClient().getQueueReference(this.flowsQueue).retrieveMessages(32);

        for (CloudQueueMessage cloudQueueMessage : messagges) {

            assertTrue(cloudQueueMessage.getMessageContentAsString().contains(p1.getOptionId())
                    || cloudQueueMessage.getMessageContentAsString().contains(p2.getOptionId())
                    || cloudQueueMessage.getMessageContentAsString().contains(p3.getOptionId()));
        }
    }

    @Test
    void booleanResponseModelTest() {

        BooleanResponseModel response = new BooleanResponseModel();

        response.setResult(Boolean.TRUE);

        assertEquals(Boolean.TRUE, response.getResult());
    }

}
