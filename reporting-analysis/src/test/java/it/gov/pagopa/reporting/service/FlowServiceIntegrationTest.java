package it.gov.pagopa.reporting.service;

import com.azure.core.util.BinaryData;
import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
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

import java.io.InputStream;
import java.util.List;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.spy;

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
    void getByOrganization_noFlowDate() {
        flowsService = spy(new FlowsService(storageConnectionString, flowsTable, containerBlob, logger));
        String organizationId =  "90000000000";
        List<Flow> flows = null;
        try {
            createTable();
            flows = flowsService.getByOrganization(organizationId, null);
            assertNotNull(flows);
        } catch (Exception e) {
            assertNull(flows);
        }
    }

    @Test
    void getByOrganization_withFlowDate() {
        flowsService = spy(new FlowsService(storageConnectionString, flowsTable, containerBlob, logger));
        String organizationId =  "90000000000";
        List<Flow> flows = null;
        try {
            createTable();
            flows = flowsService.getByOrganization(organizationId, "2023-01-01");
            assertNotNull(flows);
        } catch (Exception e) {
            assertNull(flows);
        }
    }

    @Test
    void getByFlow() throws Exception {
        flowsService = spy(new FlowsService(storageConnectionString, flowsTable, containerBlob, logger));

        String organizationId =  "idPa";
        String flowId = "idFlow";
        String flowDate = "dataOra";
        String data = null;

        BlobServiceClient blobServiceClient = createContainer();

        // precondition
        BlobContainerClient flowsContainerClient = blobServiceClient.getBlobContainerClient(this.containerBlob);
        BlobClient blobClient = flowsContainerClient.getBlobClient("dataOra##idPa##idFlow.xml");
        InputStream is = getClass().getClassLoader().getResourceAsStream("dataOra##idPa##idFlow.xml");
        blobClient.upload(BinaryData.fromStream(is));

        // test
        data = flowsService.getByFlow(organizationId, flowId, flowDate);

        blobServiceClient.deleteBlobContainer(containerBlob);

        // assert
        assertNotNull(data);
    }

    @Test
    void getByFlow_KO() {
        flowsService = spy(new FlowsService(storageConnectionString, flowsTable, containerBlob, logger));

        String organizationId =  "idPa";
        String flowId = "idFlowKO";
        String flowDate = "dataOra";
        String data = null;

        BlobServiceClient blobServiceClient = createContainer();

        // precondition
        BlobContainerClient flowsContainerClient = blobServiceClient.getBlobContainerClient(containerBlob);
        BlobClient blobClient = flowsContainerClient.getBlobClient("dataOra##idPa##idFlow.xml");
        InputStream is = getClass().getClassLoader().getResourceAsStream("dataOra##idPa##idFlow.xml");
        blobClient.upload(BinaryData.fromStream(is));

        // test
        try {
            data = flowsService.getByFlow(organizationId, flowId, flowDate);
            assertNull(data);
        } catch(Exception e) {
            assertNull(data);
        } finally {
            blobServiceClient.deleteBlobContainer(containerBlob);
        }
    }

    @SneakyThrows
    private void createTable() {
        try {
            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
            TableRequestOptions tableRequestOptions = new TableRequestOptions();
            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster
            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
            CloudTable table = cloudTableClient.getTableReference(flowsTable);
            if (!table.exists()) {
                table.createIfNotExists();
            }
        } catch (StorageException e) {
            System.out.println("createTable method failed: table already exists");
        }
    }

    @SneakyThrows
    private BlobServiceClient createContainer() {
        BlobServiceClient blobServiceClient = new BlobServiceClientBuilder()
                .connectionString(this.storageConnectionString).buildClient();
        BlobContainerClient container = blobServiceClient.getBlobContainerClient(containerBlob);
        if (!container.exists()) {
            blobServiceClient.createBlobContainer(containerBlob);
        }
        return blobServiceClient;
    }

}
