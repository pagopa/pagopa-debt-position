package it.gov.pagopa.reporting;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.azure.functions.ExecutionContext;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.RetryNoRetry;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueueClient;
import com.microsoft.azure.storage.queue.CloudQueueMessage;
import com.microsoft.azure.storage.queue.QueueRequestOptions;
import com.microsoft.azure.storage.table.*;
import it.gov.pagopa.reporting.entity.OrganizationEntity;
import it.gov.pagopa.reporting.models.OrganizationsMessage;
import it.gov.pagopa.reporting.service.OrganizationsService;
import lombok.SneakyThrows;
import org.junit.ClassRule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@Testcontainers
@ExtendWith(MockitoExtension.class)
class OrganizationsServiceIntegrationTest {

    @ClassRule
    @Container
    public static GenericContainer<?> azurite = new GenericContainer<>(
        DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest")).withExposedPorts(10001, 10002, 10000);
    Logger logger = Logger.getLogger("testlogging");

    String storageConnectionString = String.format(
            "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;TableEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1",
            azurite.getContainerIpAddress(), azurite.getMappedPort(10002),
            azurite.getContainerIpAddress(), azurite.getMappedPort(10001));
    String orgsTable = "orgtable";
    String orgsQueue = "orgqueue";

    @Spy
    RetrieveOrganizations function;

    @Mock
    ExecutionContext context;

    private static final List<String> ENROLLED_ORGANIZATIONS = List.of("90000000001", "90000000002", "90000000003");

    @SneakyThrows
    @BeforeEach
    void beforeEach() {
        TableRequestOptions tableRequestOptions = new TableRequestOptions();
        tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster

        CloudTableClient cloudTableClient = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient();
        cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
        try {
            cloudTableClient.getTableReference(this.orgsTable).createIfNotExists();
        } catch (Exception e) {
            logger.info("Table creation: no table");
        }

        QueueRequestOptions queueRequestOptions = new QueueRequestOptions();
        queueRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster

        CloudQueueClient cloudQueueClient = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient();
        cloudQueueClient.setDefaultRequestOptions(queueRequestOptions);
        try {
            cloudQueueClient.getQueueReference(this.orgsQueue).createIfNotExists();
        } catch (Exception e) {
            logger.info("Queue creation: no queue");
            e.printStackTrace();
        }
    }

    @SneakyThrows
    @AfterEach
    void afterEach() {
        TableRequestOptions tableRequestOptions = new TableRequestOptions();
        tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster

        CloudTableClient cloudTableClient = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient();
        cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
        try {
            CloudTable table = cloudTableClient.getTableReference(this.orgsTable);
            TableBatchOperation batchOperation = new TableBatchOperation();
            ENROLLED_ORGANIZATIONS.forEach(organization -> batchOperation.delete(new OrganizationEntity(organization)));
            table.execute(batchOperation);
        } catch (Exception e) {
            logger.info("Table deletion: no table");
        }

        QueueRequestOptions queueRequestOptions = new QueueRequestOptions();
        queueRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster

        CloudQueueClient cloudQueueClient = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient();
        cloudQueueClient.setDefaultRequestOptions(queueRequestOptions);
        try {
            cloudQueueClient.getQueueReference(this.orgsQueue).deleteIfExists();
        } catch (Exception e) {
            logger.info("Queue deletion: no queue");
        }
    }

    @Test
    void getOrganizationsTest() throws InvalidKeyException, URISyntaxException, StorageException {

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString, this.orgsTable,
                this.orgsQueue, 60, 0, logger);

        // simulating the organization enrolled with orgs-enrollment service
        addOrganizationList(ENROLLED_ORGANIZATIONS);

        organizationsService.getOrganizations().forEach(organization -> Assertions.assertTrue(ENROLLED_ORGANIZATIONS.contains(organization)));
    }

    @Test
    void getOrganizationsTest_noEnrolledOrganizations() throws InvalidKeyException, URISyntaxException, StorageException {

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString, this.orgsTable,
                this.orgsQueue, 60, 0, logger);

        // simulating that no organization was enrolled with orgs-enrollment service
        CloudTable table = CloudStorageAccount.parse(this.storageConnectionString)
                .createCloudTableClient()
                .getTableReference(this.orgsTable);
        Iterable<OrganizationEntity> organizationEntities = table.execute(
                TableQuery.from(OrganizationEntity.class)
                        .where(TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, OrganizationEntity.ORGANIZATION_KEY))
                );
        Assertions.assertFalse(organizationEntities.iterator().hasNext());

        List<String> retrievedOrganizations = organizationsService.getOrganizations();
        Assertions.assertEquals(new ArrayList<>(), retrievedOrganizations);
    }

    @Test
    void getOrganizationsTest_errorWrongTable() throws InvalidKeyException, URISyntaxException, StorageException {

        // simulating an exception during reading from non-existent storage table
        String wrongOrgsTable = this.orgsTable + "_fake";
        OrganizationsService organizationsService = spy(new OrganizationsService(this.storageConnectionString, wrongOrgsTable,
                this.orgsQueue, 60, 0, logger));

        // simulating an exception during reading from storage table
        List<String> retrievedOrganizations = organizationsService.getOrganizations();
        Assertions.assertEquals(new ArrayList<>(), retrievedOrganizations);
    }

    @Test
    void addToOrganizationsQueueTest() throws InvalidKeyException, URISyntaxException, StorageException, JsonProcessingException {

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString,
                this.orgsTable, this.orgsQueue, 60, 0, logger);

        // inserting on queue a set of organization identifier previously get by
        organizationsService.addToOrganizationsQueue(ENROLLED_ORGANIZATIONS);

        Iterable<CloudQueueMessage> queueMsgs = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.orgsQueue)
                .retrieveMessages(32);

        for (CloudQueueMessage queueMsg : queueMsgs) {
            Assertions.assertTrue(assertQueueMessages(queueMsg, ENROLLED_ORGANIZATIONS));
        }
    }

    @Test
    void addToOrganizationsQueueTest_errorWrongQueue() throws InvalidKeyException, URISyntaxException, StorageException, JsonProcessingException {

        Logger mockLogger = mock(Logger.class);

        // simulating an exception during reading from non-existent storage queue
        String wrongOrgsQueue = this.orgsQueue + "_fake";
        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString,
                this.orgsTable, wrongOrgsQueue, 60, 0, mockLogger);

        // inserting on queue a set of organization identifier previously get by
        organizationsService.addToOrganizationsQueue(ENROLLED_ORGANIZATIONS);

        Iterable<CloudQueueMessage> queueMsgs = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.orgsQueue)
                .retrieveMessages(32);
        Assertions.assertFalse(queueMsgs.iterator().hasNext());
        verify(mockLogger).severe(anyString());
    }

    @Test
    void retryToOrganizationsQueueTest() throws InvalidKeyException, URISyntaxException, StorageException {

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString, this.orgsTable,
                this.orgsQueue, 60, 0, logger);

        organizationsService.retryToOrganizationsQueue("90000000001", 0);

        Iterable<CloudQueueMessage> messages = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.orgsQueue)
                .retrieveMessages(32);

        List<Boolean> results = new ArrayList<>();
        for (CloudQueueMessage cloudQueueMessage : messages) {
            boolean res = cloudQueueMessage.getMessageContentAsString().contains("90000000001");
            results.add(res);
        }
        assertTrue(results.stream().allMatch(r -> r.equals(Boolean.TRUE)));
    }

    @Test
    void getAndAddOrganizations() throws Exception {

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString, this.orgsTable,
                this.orgsQueue, 60, 0, logger);

        Logger logger = Logger.getLogger("testlogging");

        // precondition
        when(context.getLogger()).thenReturn(logger);
        doReturn(organizationsService).when(function).getOrganizationsServiceInstance(logger);

        // simulating the organization enrolled with orgs-enrollment service
        addOrganizationList(ENROLLED_ORGANIZATIONS);

        // calling Azure function
        function.run("ReportingBatchTrigger", context);

        // retrieving queue messages and assert them
        Iterable<CloudQueueMessage> queueMsgs = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.orgsQueue)
                .retrieveMessages(32);
        for (CloudQueueMessage queueMsg : queueMsgs) {
            Assertions.assertTrue(assertQueueMessages(queueMsg, ENROLLED_ORGANIZATIONS));
        }
    }

    @Test
    void getAndAddOrganizations_noEnrolledOrganizations() throws Exception {

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString, this.orgsTable,
                this.orgsQueue, 60, 0, logger);

        Logger logger = Logger.getLogger("testlogging");

        // precondition
        when(context.getLogger()).thenReturn(logger);
        doReturn(organizationsService).when(function).getOrganizationsServiceInstance(logger);

        // calling Azure function
        function.run("ReportingBatchTrigger", context);

        // retrieving queue messages and assert them
        Iterable<CloudQueueMessage> queueMsgs = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.orgsQueue)
                .retrieveMessages(32);
        Assertions.assertTrue(!queueMsgs.iterator().hasNext());
    }

    private void addOrganizationList(List<String> organizations) throws URISyntaxException, InvalidKeyException, StorageException {
        CloudTable table = CloudStorageAccount.parse(this.storageConnectionString)
                .createCloudTableClient()
                .getTableReference(this.orgsTable);
        TableBatchOperation batchOperation = new TableBatchOperation();
        organizations.forEach(organization -> batchOperation.insert(new OrganizationEntity(organization, LocalDateTime.now().toString())));
        table.execute(batchOperation);
    }

    private static boolean assertQueueMessages(CloudQueueMessage queueMsg, List<String> enrolledOrganizations) throws StorageException {
        boolean result = false;
        Iterator<String> it = enrolledOrganizations.iterator();
        while (!result && it.hasNext()) {
            result = queueMsg.getMessageContentAsString().contains(it.next());
        }
        return result;
    }
}
