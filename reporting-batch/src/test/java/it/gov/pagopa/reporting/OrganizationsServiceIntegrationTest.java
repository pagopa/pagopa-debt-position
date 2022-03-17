package it.gov.pagopa.reporting;

import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.RetryNoRetry;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.StorageExtendedErrorInformation;
import com.microsoft.azure.storage.queue.CloudQueueClient;
import com.microsoft.azure.storage.queue.CloudQueueMessage;
import com.microsoft.azure.storage.queue.QueueRequestOptions;
import com.microsoft.azure.storage.table.*;
import it.gov.pagopa.reporting.entity.OrganizationEntity;
import it.gov.pagopa.reporting.models.Organization;
import it.gov.pagopa.reporting.models.Organizations;
import it.gov.pagopa.reporting.service.OrganizationsService;
import lombok.SneakyThrows;
import org.junit.ClassRule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.shaded.com.google.common.base.Predicates;
import org.testcontainers.utility.DockerImageName;

import javax.persistence.criteria.Predicate;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@Testcontainers
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
            logger.info("no table");
        }

        QueueRequestOptions queueRequestOptions = new QueueRequestOptions();
        queueRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster

        CloudQueueClient cloudQueueClient = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient();
        cloudQueueClient.setDefaultRequestOptions(queueRequestOptions);
        try {
            cloudQueueClient.getQueueReference(this.orgsQueue).createIfNotExists();
        } catch (Exception e) {
            logger.info("no queue");
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
            cloudTableClient.getTableReference(this.orgsTable).deleteIfExists();
        } catch (Exception e) {
            logger.info("no table");
        }

        QueueRequestOptions queueRequestOptions = new QueueRequestOptions();
        queueRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster

        CloudQueueClient cloudQueueClient = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient();
        cloudQueueClient.setDefaultRequestOptions(queueRequestOptions);
        try {
            cloudQueueClient.getQueueReference(this.orgsQueue).deleteIfExists();
        } catch (Exception e) {
            logger.info("no queue");
        }
    }

    @Test
    void addOrganizationListTest() throws InvalidKeyException,
            URISyntaxException, StorageException {
//        CloudStorageAccount.parse(storageConnectionString).createCloudTableClient().getTableReference(this.orgsTable)
//                .createIfNotExists();

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString, this.orgsTable,
                this.orgsQueue, 60, 0, logger);

        Organizations orgs = new Organizations();

        List<Organization> added = new ArrayList<>();
        added.add(new Organization("90000000001"));
        added.add(new Organization("90000000002"));
        added.add(new Organization("90000000003"));
        orgs.setAdd(added);
        List<Organization> deleted = new ArrayList<>();
        orgs.setDelete(deleted);

        organizationsService.processOrganizationList(orgs);

        TableQuery<OrganizationEntity> query = new TableQuery<OrganizationEntity>();

        Iterable<OrganizationEntity> rows = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.orgsTable)
                .execute(TableQuery.from(OrganizationEntity.class).where((TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, "organization"))));

        List<String> chk = new ArrayList<>();
        chk.add("90000000001");
        chk.add("90000000002");
        chk.add("90000000003");
        chk.add("90000000004");
        int index = 0;
        for (OrganizationEntity r : rows) {
            Assertions.assertTrue(r.getRowKey().contains(chk.get(index++)));
        }
    }

    @Test
    void addOrganizationListTest_2() throws InvalidKeyException, URISyntaxException, StorageException {
//        CloudStorageAccount.parse(storageConnectionString).createCloudTableClient().getTableReference(this.orgsTable)
//                .createIfNotExists();

        OrganizationsService organizationsService = spy(new OrganizationsService(this.storageConnectionString, this.orgsTable,
                this.orgsQueue, 60, 0, logger));

        StorageExtendedErrorInformation err = new StorageExtendedErrorInformation();
        err.setErrorMessage("test error message");
        doThrow(new TableServiceException("error", "message", 500, err, null)).when(organizationsService).addOrganizationList(any());

        Organizations orgs = new Organizations();

        List<Organization> added = new ArrayList<>();
        added.add(new Organization("90000000001"));
        added.add(new Organization("90000000002"));
        added.add(new Organization("90000000003"));
        orgs.setAdd(added);
        List<Organization> deleted = new ArrayList<>();
        orgs.setDelete(deleted);

        organizationsService.processOrganizationList(orgs);

        verify(organizationsService, times(3)).addOrganization(anyString());
    }

    @Test
    void addOrganizationListTestSingle() throws InvalidKeyException, URISyntaxException, StorageException {

//        CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient().getQueueReference(this.orgsQueue)
//                .createIfNotExists();
//
//        CloudStorageAccount.parse(storageConnectionString).createCloudTableClient().getTableReference(this.orgsTable)
//                .createIfNotExists();

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString, this.orgsTable,
                this.orgsQueue, 60, 0, logger);

        Organizations orgs = new Organizations();

        List<Organization> added = new ArrayList<>();
        added.add(new Organization("90000000001"));
        added.add(new Organization("90000000002"));
        added.add(new Organization("90000000003"));
        added.add(new Organization("90000000004"));
        orgs.setAdd(added);
        List<Organization> deleted = new ArrayList<>();
        orgs.setDelete(deleted);

        organizationsService.processOrganizationList(orgs);

        TableQuery<OrganizationEntity> query = new TableQuery<OrganizationEntity>();

        Iterable<OrganizationEntity> rows = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.orgsTable)
                .execute(TableQuery.from(OrganizationEntity.class).where((TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, "organization"))));

        List<String> chk = new ArrayList<>();
        chk.add("90000000001");
        chk.add("90000000002");
        chk.add("90000000003");
        chk.add("90000000004");
        int index = 0;
        for (OrganizationEntity r : rows) {
            Assertions.assertTrue(r.getRowKey().contains(chk.get(index++)));
        }

    }

    @Test
    void delOrganizationListTest() {

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString,
                this.orgsTable, this.orgsQueue, 60, 0, logger);

        Organizations orgs = new Organizations();

        List<Organization> added = new ArrayList<>();
        orgs.setAdd(added);

        List<Organization> deleted = new ArrayList<>();
        deleted.add(new Organization("90000000001"));
        deleted.add(new Organization("90000000002"));
        orgs.setDelete(deleted);

        List<String> updateOrganizationsList = organizationsService.processOrganizationList(orgs); // delete ------------

        List<String> chk1 = new ArrayList<>();
        chk1.add("90000000003");
        chk1.add("90000000004");
        int index1 = 0;
        for (String org : updateOrganizationsList) {
            assertEquals(org, chk1.get(index1++));
        }
    }

    @Test
    void delOrganizationLisNotExistTest() {

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString,
                this.orgsTable, this.orgsQueue, 60, 0, logger);

        Organizations orgs = new Organizations();

        List<Organization> added = new ArrayList<>();
        orgs.setAdd(added);

        List<Organization> deleted = new ArrayList<>();
        deleted.add(new Organization("90000000001"));
        deleted.add(new Organization("90000000001"));

        orgs.setDelete(deleted);

        List<String> updateOrganizationsList = organizationsService.processOrganizationList(orgs); // delete ------------

        List<String> chk1 = new ArrayList<>();
        chk1.add("90000000002");
        chk1.add("90000000003");
        chk1.add("90000000004");
        int index1 = 0;
        for (String org : updateOrganizationsList) {
            assertEquals(org, chk1.get(index1++));
        }

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
    void addToOrganizationsQueueTest()  throws InvalidKeyException, URISyntaxException, StorageException {

        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString,
                this.orgsTable, this.orgsQueue, 60, 0, logger);

        List<String> orgs = new ArrayList<>();
        orgs.add("90000000001");
        orgs.add("90000000002");
        orgs.add("90000000003");

        /**
         * Test
         */
        organizationsService.addToOrganizationsQueue(orgs);

        /**
         * Asserts
         */

        Iterable<CloudQueueMessage> messagges = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.orgsQueue)
                .retrieveMessages(32);

        for (CloudQueueMessage cloudQueueMessage : messagges) {
            Assertions.assertTrue(cloudQueueMessage.getMessageContentAsString().contains(orgs.get(0))
                    || cloudQueueMessage.getMessageContentAsString().contains(orgs.get(1))
                    || cloudQueueMessage.getMessageContentAsString().contains(orgs.get(2)));
        }

    }
}
