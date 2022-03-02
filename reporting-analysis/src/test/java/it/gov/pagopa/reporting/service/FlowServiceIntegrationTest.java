package it.gov.pagopa.reporting.service;

import static org.mockito.Mockito.*;

import java.io.InputStream;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.List;
import java.util.logging.Logger;

import com.azure.core.util.BinaryData;
import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.RetryNoRetry;
import com.microsoft.azure.storage.StorageException;

import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.CloudTableClient;
import com.microsoft.azure.storage.table.TableRequestOptions;
import it.gov.pagopa.reporting.model.Flow;

import lombok.SneakyThrows;
import org.junit.ClassRule;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;


@Testcontainers
class FlowServiceIntegrationTest {
    @ClassRule
    @Container
    public static GenericContainer<?> azurite = new GenericContainer<>(
            DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest")).withExposedPorts(10001, 10002,
            10000);

    Logger logger = Logger.getLogger("testlogging");

    String storageConnectionString = String.format(
            "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;BlobEndpoint=http://%s:%s/devstoreaccount1;TableEndpoint=http://%s:%s/devstoreaccount1",
            azurite.getContainerIpAddress(), azurite.getMappedPort(10000),
            azurite.getContainerIpAddress(), azurite.getMappedPort(10002));

    String flowsTable = "flows";
    String containerBlob = "blob";

    FlowsService flowsService;

    @Test
    void getByOrganization() {

        flowsService = spy(new FlowsService(storageConnectionString, flowsTable, containerBlob, logger));

        String organizationId =  "90000000000";
        try {
            createTable();
            List<Flow> flows = flowsService.getByOrganization(organizationId);
        } catch (InvalidKeyException | URISyntaxException | StorageException | JsonProcessingException e) {
            assert(false);
        }
        assert(true);
    }

    @Test
    void getByFlow() {
        flowsService = spy(new FlowsService(storageConnectionString, flowsTable, containerBlob, logger));

        String organizationId =  "idPa";
        String flowId = "idFlow";
        String flowDate = "dataOra";
        try {
            BlobServiceClient blobServiceClient = createContainer();

            // precondition
            BlobContainerClient flowsContainerClient = blobServiceClient.getBlobContainerClient(this.containerBlob);
            BlobClient blobClient = flowsContainerClient.getBlobClient("dataOra##idPa##idFlow.xml");
            InputStream is = getClass().getClassLoader().getResourceAsStream("dataOra##idPa##idFlow.xml");
            blobClient.upload(BinaryData.fromStream(is));

            // test
            String data = flowsService.getByFlow(organizationId, flowId, flowDate);
        } catch (InvalidKeyException | URISyntaxException | StorageException | JsonProcessingException e) {
            assert(false);
        }
        assert(true);
    }

    @SneakyThrows
    private void createTable() {
        CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
        CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
        TableRequestOptions tableRequestOptions = new TableRequestOptions();
        tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster
        cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
        CloudTable table = cloudTableClient.getTableReference(flowsTable);
        table.createIfNotExists();
    }

    @SneakyThrows
    private BlobServiceClient createContainer() {
        BlobServiceClient blobServiceClient = new BlobServiceClientBuilder()
                .connectionString(this.storageConnectionString).buildClient();
        blobServiceClient.createBlobContainer(containerBlob);
        return blobServiceClient;
    }

//    @Test
//    void flowsProcessingTest() throws Exception {
//
//        /**
//         * Mock input - identical flows
//         */
//        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
//
//        GregorianCalendar cal2 = new GregorianCalendar();
//        cal2.setTime(format.parse("2015-04-24 11:15:00"));
//
//        TipoIdRendicontazione e1 = new TipoIdRendicontazione();
//        e1.setIdentificativoFlusso(UUID.randomUUID().toString());
//        GregorianCalendar cal1 = new GregorianCalendar();
//        cal1.setTime(format.parse("2014-04-24 11:15:00"));
//        e1.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
//                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));
//
//        TipoElencoFlussiRendicontazione elencoFlussi = new TipoElencoFlussiRendicontazione();
//        elencoFlussi.setTotRestituiti(2);
//        elencoFlussi.getIdRendicontazione().add(e1);
//        elencoFlussi.getIdRendicontazione().add(e1);
//
//        FlowsService flowsService = Mockito.spy(
//                new FlowsService("connectionStringMock", "tableMock", "queueMock", Logger.getLogger("testlogging")));
//
//        /**
//         * Precondition
//         */
//        doThrow(new TableServiceException("InvalidDuplicateRow", "message InvalidDuplicateRow", 400,
//                new StorageExtendedErrorInformation(), null)).when(flowsService).flowsBatchProcessing(any(),
//                        anyString(), anyInt());
//        doNothing().when(flowsService).flowProcessing(any(), anyString());
//
//        /**
//         * Test
//         */
//        flowsService.flowsProcessing(elencoFlussi.getIdRendicontazione(), "idPaMock");
//
//        /**
//         * Asserts
//         */
//        verify(flowsService, times(1)).flowsBatchProcessing(any(), anyString(), anyInt());
//        verify(flowsService, times(2)).flowProcessing(any(), anyString());
//    }

}
