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
import it.gov.pagopa.reporting.models.Organizations;
import it.gov.pagopa.reporting.models.OrganizationsMessage;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.IntStream;

public class OrganizationsService {
//    private boolean debugAzurite = Boolean.parseBoolean(System.getenv("DEBUG_AZURITE"));

    private String storageConnectionString;
    private String organizationsTable;
    private String organzagionsQueue;
    private Logger logger;
    private int batchSize = 2;
    private int msgOrganizationsForQueue = 5;

    public OrganizationsService(String storageConnectionString, String organizationsTable, String organzagionsQueue, Logger logger) {
        this.storageConnectionString = storageConnectionString;
        this.organizationsTable = organizationsTable;
        this.organzagionsQueue = organzagionsQueue;
        this.logger = logger;
    }

    private void createTable() throws URISyntaxException, InvalidKeyException, StorageException {
        // Create a new table
        CloudTable table = CloudStorageAccount.parse(storageConnectionString)
                .createCloudTableClient().getTableReference(this.organizationsTable);
        if (!table.exists()) {
            table.createIfNotExists();
        }
    }

    private void createQueue() throws URISyntaxException, InvalidKeyException, StorageException {
        // Create a new queue
        CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.organzagionsQueue);
        queue.createIfNotExists();
    }

    public List<String> processOrganizationList(Organizations organizations) {
        this.logger.info("Processing organization list");

        if (false) { // debugazurite
            try {
                createTable();
            } catch (Exception e) {
                this.logger.severe(String.format("[OrganizationsService] Problem to retrieve organization list: %s", e.getLocalizedMessage()));
                return new ArrayList<>();
            }

            try {
                createQueue();
            } catch (URISyntaxException | InvalidKeyException | StorageException e ) {
                this.logger.severe(String.format("[OrganizationsService] Problem to retrieve organization list: %s", e.getLocalizedMessage()));
                e.printStackTrace();
            }
        }


        // create batch partition due to max batch size of Azure Table Storage - 100
        List<List<String>> addOrganizationList = Lists.partition(organizations.getAdd(), batchSize);
        List<List<String>> deleteOrganizationList = Lists.partition(organizations.getDelete(), batchSize);

        // add organizations section
        IntStream.range(0, addOrganizationList.size()).forEach(partitionAddIndex -> {
            try {
                this.addOrganizationList(addOrganizationList.get(partitionAddIndex));
            } catch (TableServiceException et) {
                this.logger.log(Level.SEVERE,
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
        for (OrganizationEntity entity : table.execute(TableQuery.from(OrganizationEntity.class).where((TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, OrganizationEntity.organizationKey))))) {
            // this.logger.info(String.format("\tOrganizationEntity: %s,%s", entity.getPartitionKey(), entity.getRowKey()));
            organizationsList.add(entity.getRowKey());
        }

        return organizationsList;
    }

    // Organizations table
    public void addOrganizationList(List<String> organizations) throws URISyntaxException, InvalidKeyException, StorageException {
        this.logger.info("Processing add organization list");

        CloudTable table = CloudStorageAccount.parse(storageConnectionString)
                .createCloudTableClient()
                .getTableReference(this.organizationsTable);

        TableBatchOperation batchOperation = new TableBatchOperation();

        organizations.forEach(organization -> batchOperation.insert(new OrganizationEntity(organization, LocalDateTime.now().toString())));

        table.execute(batchOperation);
    }

    public void addOrganization(String organization) throws URISyntaxException, InvalidKeyException, StorageException {
        this.logger.info("Processing add organization " + organization);
        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.organizationsTable);

        table.execute(TableOperation.insert(new OrganizationEntity(organization, LocalDateTime.now().toString())));
    }

    public void deleteOrganizationList(List<String> organizations) throws URISyntaxException, InvalidKeyException, StorageException {
        this.logger.info("Processing delete organization list");

        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.organizationsTable);

        TableBatchOperation batchOperation = new TableBatchOperation();

        organizations.forEach(organization -> batchOperation.delete(new OrganizationEntity(organization)));

        table.execute(batchOperation);

    }

    public void deleteOrganization(String organization) throws URISyntaxException, InvalidKeyException, StorageException {
        this.logger.info("Processing delete organization " + organization);
        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.organizationsTable);

        table.execute(TableOperation.delete(new OrganizationEntity(organization)));
    }

    // Organizations queue

    public void addToOrganizationsQueue(List<String> organizations) {

        this.logger.log(Level.INFO, () -> "[OrganizationsService] addToOrganizationsQueue ");

        try {
            final CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                    .getQueueReference(this.organzagionsQueue);

            List<List<String>> queueMsgOrganizations = Lists.partition(organizations, msgOrganizationsForQueue);

            IntStream.range(0, queueMsgOrganizations.size()).forEach(partitionMsgIndex -> {
                // set single message
                OrganizationsMessage organizationsMessage = new OrganizationsMessage();
                organizationsMessage.setIdPA(queueMsgOrganizations.get(partitionMsgIndex).stream().toArray(String[]::new));

                try {
                    String message = new ObjectMapper().writeValueAsString(organizationsMessage);
                    this.logger.info("[OrganizationsService] Sending " + partitionMsgIndex + " " + message + " to organizationsQueue");
                    queue.addMessage(new CloudQueueMessage(message));

                } catch (JsonProcessingException | StorageException e) {
                    this.logger.log(Level.SEVERE, () -> "[OrganizationsService]  Error " + e.getLocalizedMessage());
                }

            });

        } catch (URISyntaxException | StorageException | InvalidKeyException e) {
            this.logger.log(Level.SEVERE, () -> "[OrganizationsService]  Error " + e.getLocalizedMessage());
        }






    }


}
