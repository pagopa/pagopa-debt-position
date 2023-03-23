package it.gov.pagopa.reporting.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueue;
import com.microsoft.azure.storage.queue.CloudQueueMessage;
import com.microsoft.azure.storage.table.*;
import it.gov.pagopa.reporting.entity.OrganizationEntity;
import it.gov.pagopa.reporting.models.OrganizationsMessage;
import it.gov.pagopa.reporting.utils.AzuriteStorageUtil;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.IntStream;

public class OrganizationsService {

    private final String storageConnectionString;
    private final String organizationsTable;
    private final String organizationsQueue;
    // timeToLiveInSeconds – The maximum time to allow the message to be in the queue.
    //      A value of zero will set the time-to-live to the service default value of seven days.
    //      A value of negative one will set an infinite time-to-live.
    private final int timeToLiveInSeconds;
    // initialVisibilityDelayInSeconds – The length of time during which the message will be invisible, starting when
    //      it is added to the queue, or 0 to make the message visible immediately. This value must be greater than or
    //      equal to zero and less than the time-to-live value.
    private final int initialVisibilityDelayInSeconds;

    private final Logger logger;
    private static final int MAX_ORGANIZATIONS_FOR_EACH_QUEUE_MSG = 5;

    public OrganizationsService(String storageConnectionString, String organizationsTable, String organizationsQueue, int timeToLiveInSeconds, int initialVisibilityDelayInSeconds, Logger logger) {
        this.storageConnectionString = storageConnectionString;
        this.organizationsTable = organizationsTable;
        this.organizationsQueue = organizationsQueue;
        this.timeToLiveInSeconds = timeToLiveInSeconds;
        this.initialVisibilityDelayInSeconds = initialVisibilityDelayInSeconds;
        this.logger = logger;
    }



    public List<String> getOrganizations() {
        this.logger.info("[OrganizationsService] Retrieving organizations");
        ArrayList<String> organizationsList = new ArrayList<>();
        try {
            // try to create Azure table and queue, then it take it
            createEnv();
            CloudTable table = CloudStorageAccount.parse(storageConnectionString)
                    .createCloudTableClient()
                    .getTableReference(this.organizationsTable);

            // Iterate through the results
            Iterable<OrganizationEntity> organizationEntities = table.execute(
                    TableQuery.from(OrganizationEntity.class)
                    .where(TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, OrganizationEntity.ORGANIZATION_KEY))
            );
            organizationEntities.forEach(organizationEntity -> organizationsList.add(organizationEntity.getRowKey()));
        } catch (Exception e) {
            this.logger.severe(String.format("[OrganizationsService] Problem to retrieve organizations: %s", e.getLocalizedMessage()));
        }
        return organizationsList;
    }

    // Organizations queue
    public void addToOrganizationsQueue(List<String> organizations) {
        this.logger.info("[OrganizationsService] Adding organizations to queue");
        try {
            // retrieving queue object for next insertions
            final CloudQueue queue = CloudStorageAccount.parse(storageConnectionString)
                    .createCloudQueueClient()
                    .getQueueReference(this.organizationsQueue);

            List<List<String>> partitionedOrganizations = Lists.partition(organizations, MAX_ORGANIZATIONS_FOR_EACH_QUEUE_MSG);
            IntStream.range(0, partitionedOrganizations.size()).forEach(partitionMsgIndex -> {

                // set single message
                OrganizationsMessage organizationsMessage = OrganizationsMessage.builder()
                        .idPA(partitionedOrganizations.get(partitionMsgIndex).toArray(String[]::new))
                        .retry(0)
                        .build();

                // generating message and sending it to queue
                try {
                    String message = new ObjectMapper().writeValueAsString(organizationsMessage);
                    this.logger.info("[OrganizationsService] Sending " + partitionMsgIndex + " " + message + " to organizationsQueue");
                    queue.addMessage(new CloudQueueMessage(message), timeToLiveInSeconds, 0, null, null);
                } catch (JsonProcessingException | StorageException e) {
                    this.logger.severe("[OrganizationsService]  Error " + e.getLocalizedMessage());
                }
            });

        } catch (URISyntaxException | StorageException | InvalidKeyException e) {
            this.logger.log(Level.SEVERE, () -> "[OrganizationsService]  Error " + e.getLocalizedMessage());
        }
    }

    public void retryToOrganizationsQueue(String organization, Integer retry) {

        this.logger.log(Level.INFO, () -> String.format("[OrganizationsService] retryToOrganizationsQueue %s with retry %s", organization, retry));

        try {
            final CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                    .getQueueReference(this.organizationsQueue);

            // set single message
            OrganizationsMessage organizationsMessage = new OrganizationsMessage();
            organizationsMessage.setIdPA(new String[]{organization});
            organizationsMessage.setRetry(retry);

            String message = new ObjectMapper().writeValueAsString(organizationsMessage);
            queue.addMessage(new CloudQueueMessage(message), timeToLiveInSeconds, initialVisibilityDelayInSeconds, null, null);

        } catch (URISyntaxException | InvalidKeyException | JsonProcessingException | StorageException e) {
            this.logger.log(Level.SEVERE, () -> "[OrganizationsService] Error " + e.getLocalizedMessage());
        }
    }

    private void createEnv() {
        AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, organizationsTable, organizationsQueue);
        try {
            azuriteStorageUtil.createTable();
            azuriteStorageUtil.createQueue();
        } catch (StorageException e) {
            this.logger.info(String.format("[AzureStorage] Table or Queue created: %s", e.getMessage()));
        } catch (Exception e) {
            this.logger.severe(String.format("[AzureStorage] Problem to create table or queue: %s", e.getMessage()));
        }
    }


}
