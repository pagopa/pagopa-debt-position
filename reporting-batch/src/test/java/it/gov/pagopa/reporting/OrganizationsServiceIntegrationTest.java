package it.gov.pagopa.reporting;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueueMessage;
import com.microsoft.azure.storage.table.TableQuery;
import it.gov.pagopa.reporting.entity.OrganizationEntity;
import it.gov.pagopa.reporting.models.Organizations;
import it.gov.pagopa.reporting.service.FlowsService;
import it.gov.pagopa.reporting.service.OrganizationsService;
import it.gov.pagopa.reporting.servicewsdl.TipoElencoFlussiRendicontazione;
import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;
import org.junit.ClassRule;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import java.io.Console;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Logger;

import static org.junit.Assert.assertTrue;

@Testcontainers
class OrganizationsServiceIntegrationTest {

    @ClassRule
    @Container
    public static GenericContainer<?> azurite = new GenericContainer<>(
            DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest")).withExposedPorts(10001, 10002,
                    10000);

    Logger logger = Logger.getLogger("testlogging");

    String storageConnectionString = String.format(
            "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;TableEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1",
            azurite.getContainerIpAddress(), azurite.getMappedPort(10002), azurite.getContainerIpAddress(),
            azurite.getMappedPort(10001));
    String orgsTable = "orgtable";
    String flowsQueue = "flowqueue";

    @Test
    void addOrganizationListTest() throws ParseException, DatatypeConfigurationException, InvalidKeyException,
            URISyntaxException, StorageException, JsonProcessingException {

        CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient().getQueueReference(this.flowsQueue)
                .createIfNotExists();

        CloudStorageAccount.parse(storageConnectionString).createCloudTableClient().getTableReference(this.orgsTable)
                .createIfNotExists();


        OrganizationsService organizationsService = new OrganizationsService(this.storageConnectionString, this.orgsTable, this.flowsQueue,
                logger);


        Organizations orgs = new Organizations();

        List<String> added = new ArrayList<>();
        added.add("90000000001");
        added.add("90000000002");
        added.add("90000000003");
        orgs.setAdd(added);
        List<String> deleted = new ArrayList<>();
        orgs.setDelete(deleted);

        organizationsService.processOrganizationList(orgs);

        TableQuery<OrganizationEntity> query = new TableQuery<OrganizationEntity>();

        Iterable<OrganizationEntity> rows = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.orgsTable)
                .execute(TableQuery.from(OrganizationEntity.class).where((TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, "organization"))));

        int index = 0;
        for (OrganizationEntity r : rows) {
            assertTrue(r.getRowKey().contains(added.get(index++)));
        }

    }

}
