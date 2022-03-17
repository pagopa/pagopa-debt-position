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
import it.gov.pagopa.reporting.models.Organization;
import it.gov.pagopa.reporting.models.Organizations;
import it.gov.pagopa.reporting.models.OrganizationsMessage;
import it.gov.pagopa.reporting.utils.AzuriteStorageUtil;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
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
    private static final int batchSize = 2;
    private static final int msgOrganizationsForQueue = 5;

    public OrganizationsService(String storageConnectionString, String organizationsTable, String organizationsQueue, int timeToLiveInSeconds, int initialVisibilityDelayInSeconds, Logger logger) {
        this.storageConnectionString = storageConnectionString;
        this.organizationsTable = organizationsTable;
        this.organizationsQueue = organizationsQueue;
        this.timeToLiveInSeconds = timeToLiveInSeconds;
        this.initialVisibilityDelayInSeconds = initialVisibilityDelayInSeconds;
        this.logger = logger;
    }

    public List<String> processOrganizationList(Organizations organizations) {
        // try to create Azure table and queue
        createEnv();

        this.logger.info("[OrganizationsService] Processing organization list");

        // create batch partition due to max batch size of Azure Table Storage - 100
        List<List<String>> addOrganizationList = Lists.partition(organizations.getAdd().stream().map(Organization::getOrganizationFiscalCode).collect(Collectors.toList()), batchSize);
        List<List<String>> deleteOrganizationList = Lists.partition(organizations.getDelete().stream().map(Organization::getOrganizationFiscalCode).collect(Collectors.toList()), batchSize);

        // add organizations section
        IntStream.range(0, addOrganizationList.size()).forEach(partitionAddIndex -> {
            try {
                this.addOrganizationList(addOrganizationList.get(partitionAddIndex));
                this.logger.log(Level.INFO,
                        () -> "[OrganizationsService] Azure Table Storage - Add for partition index " + partitionAddIndex + " executed.");
            } catch (TableServiceException et) {
                this.logger.log(Level.WARNING,
                        () -> "[OrganizationsService] Azure Table Storage Error - Add:  " + et.getErrorCode() + " : "
                                + et.getExtendedErrorInformation().getErrorMessage() + " for batch of organizations");

                // try to add individually the organizations
                addOrganizationList.get(partitionAddIndex).forEach(organization -> {
                    try {
                        addOrganization(organization);
                    } catch (URISyntaxException | InvalidKeyException | StorageException e) {
                        this.logger.log(Level.SEVERE,
                                () -> "[OrganizationsService] Azure Table Storage Error - Add:  " + et.getErrorCode() + " : "
                                        + et.getExtendedErrorInformation().getErrorMessage() + " for single organization: "
                                        + organization
                        );
                    }
                });

            } catch (Exception e) {
                this.logger.severe(String.format("[OrganizationsService] Generic Error %s in add batch %s",
                        e.getLocalizedMessage(), partitionAddIndex));
            }
        });

        // delete organizations section
        IntStream.range(0, deleteOrganizationList.size()).forEach(partitionDeleteIndex -> {
            try {
                this.deleteOrganizationList(deleteOrganizationList.get(partitionDeleteIndex));
            } catch (TableServiceException et) {
                this.logger.log(Level.SEVERE,
                        () -> "[OrganizationsService] Azure Table Storage Error - Delete:  " + et.getErrorCode() + " : "
                                + et.getExtendedErrorInformation().getErrorMessage() + " for batch of organizations");

                // try to delete individually the organizations
                deleteOrganizationList.get(partitionDeleteIndex).forEach(organization -> {
                    try {
                        deleteOrganization(organization);
                    } catch (URISyntaxException | InvalidKeyException | StorageException e) {
                        this.logger.log(Level.SEVERE,
                                () -> "[OrganizationsService] Azure Table Storage Error - Delete:  " + et.getErrorCode() + " : "
                                        + et.getExtendedErrorInformation().getErrorMessage() + " for single organization: "
                                        + organization
                        );
                    }
                });

            } catch (Exception e) {
                this.logger.severe(String.format("[OrganizationsService] Generic Error %s in delete batch %s",
                        e.getLocalizedMessage(), partitionDeleteIndex));
            }
        });

        // retrieve updated organization list
        try {
            return getOrganizationList();
        } catch (Exception e) {
            this.logger.severe(String.format("[OrganizationsService] Problem to retrieve organization list: %s", e.getLocalizedMessage()));
            return new ArrayList<>();
        }
    }

    private List<String> getOrganizationList() throws URISyntaxException, InvalidKeyException, StorageException {
        CloudTable table = CloudStorageAccount.parse(storageConnectionString)
                .createCloudTableClient()
                .getTableReference(this.organizationsTable);

        ArrayList<String> organizationsList = new ArrayList<>();
        // Iterate through the results
        for (OrganizationEntity entity : table.execute(TableQuery.from(OrganizationEntity.class).where((TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, OrganizationEntity.ORGANIZATION_KEY))))) {
            organizationsList.add(entity.getRowKey());
        }

        return organizationsList;
    }

    // Organizations table
    public void addOrganizationList(List<String> organizations) throws URISyntaxException, InvalidKeyException, StorageException {
        this.logger.info("[OrganizationsService] Processing add organization list");

        CloudTable table = CloudStorageAccount.parse(storageConnectionString)
                .createCloudTableClient()
                .getTableReference(this.organizationsTable);

        TableBatchOperation batchOperation = new TableBatchOperation();

        organizations.forEach(organization -> batchOperation.insert(new OrganizationEntity(organization, LocalDateTime.now().toString())));

        table.execute(batchOperation);
    }

    public void addOrganization(String organization) throws URISyntaxException, InvalidKeyException, StorageException {
        this.logger.log(Level.INFO, () -> "[OrganizationsService] Processing add organization " + organization);

        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.organizationsTable);

        table.execute(TableOperation.insert(new OrganizationEntity(organization, LocalDateTime.now().toString())));
    }

    public void deleteOrganizationList(List<String> organizations) throws URISyntaxException, InvalidKeyException, StorageException {
        this.logger.info("[OrganizationsService] Processing delete organization list");

        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.organizationsTable);

        TableBatchOperation batchOperation = new TableBatchOperation();

        organizations.forEach(organization -> batchOperation.delete(new OrganizationEntity(organization)));

        table.execute(batchOperation);

    }

    public void deleteOrganization(String organization) throws URISyntaxException, InvalidKeyException, StorageException {
        this.logger.log(Level.INFO, () -> "Processing delete organization " + organization);

        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.organizationsTable);

        table.execute(TableOperation.delete(new OrganizationEntity(organization)));
    }

    // Organizations queue

    public void addToOrganizationsQueue(List<String> organizations) {

        this.logger.log(Level.INFO, () -> "[OrganizationsService] addToOrganizationsQueue ");

        try {
            final CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                    .getQueueReference(this.organizationsQueue);

            List<List<String>> queueMsgOrganizations = Lists.partition(organizations, msgOrganizationsForQueue);

            IntStream.range(0, queueMsgOrganizations.size()).forEach(partitionMsgIndex -> {
                // set single message
                OrganizationsMessage organizationsMessage = new OrganizationsMessage();
                organizationsMessage.setIdPA(queueMsgOrganizations.get(partitionMsgIndex).toArray(String[]::new));
                organizationsMessage.setRetry(0);

                try {
                    String message = new ObjectMapper().writeValueAsString(organizationsMessage);
                    this.logger.info("[OrganizationsService] Sending " + partitionMsgIndex + " " + message + " to organizationsQueue");
                    queue.addMessage(new CloudQueueMessage(message), timeToLiveInSeconds, 0, null, null);

                } catch (JsonProcessingException | StorageException e) {
                    this.logger.log(Level.SEVERE, () -> "[OrganizationsService]  Error " + e.getLocalizedMessage());
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
