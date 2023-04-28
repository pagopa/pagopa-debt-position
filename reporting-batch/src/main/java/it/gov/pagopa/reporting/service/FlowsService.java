package it.gov.pagopa.reporting.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueue;
import com.microsoft.azure.storage.queue.CloudQueueMessage;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.TableBatchOperation;
import com.microsoft.azure.storage.table.TableOperation;
import com.microsoft.azure.storage.table.TableServiceException;
import it.gov.pagopa.reporting.entity.FlowEntity;
import it.gov.pagopa.reporting.models.FlowsMessage;
import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;
import it.gov.pagopa.reporting.utils.AzuriteStorageUtil;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.IntStream;

public class FlowsService {

    private String storageConnectionString;
    private String flowsTable;
    private String flowsQueue;
    private Logger logger;
    private int batchSize = 2;

    public FlowsService(String storageConnectionString, String flowsTable, String flowsQueue, Logger logger) {
        this.storageConnectionString = storageConnectionString;
        this.flowsTable = flowsTable;
        this.flowsQueue = flowsQueue;
        this.logger = logger;
    }

    public void flowsProcessing(List<TipoIdRendicontazione> flows, String idPA) {

        createEnv();

        this.logger.log(Level.INFO, "[FlowsService] START flows storing ");

        /**
         * Flows partition due to max batch size of Azure Table Storage - 100
         */
        List<List<TipoIdRendicontazione>> partitionsFlows = Lists.partition(flows, batchSize);
        this.logger.log(Level.INFO, () -> "[FlowsService] " + flows.size() + " flows in " + partitionsFlows.size()
                + "  batch of size " + batchSize);

        /**
         * Scan partitions
         */
        IntStream.range(0, partitionsFlows.size()).forEach(partitionFlowsIndex -> {

            try {

                /**
                 * Partition Batch Processing
                 */
                this.flowsBatchProcessing(partitionsFlows.get(partitionFlowsIndex), idPA, partitionFlowsIndex);
            } catch (TableServiceException e) {

                this.logger.log(Level.SEVERE,
                        () -> "[FlowsService] Azure Table Storage Error:  " + e.getErrorCode() + " : "
                                + e.getExtendedErrorInformation().getErrorMessage() + " for batch "
                                + partitionFlowsIndex);

                /**
                 * Partition Individual Processing, scan flows
                 */
                partitionsFlows.get(partitionFlowsIndex).forEach(flow -> {
                    try {
                        this.flowProcessing(flow, idPA);
                    } catch (TableServiceException et) {

                        this.logger.log(Level.SEVERE,
                                () -> "[FlowsService] Azure Table Storage Error:  " + et.getErrorCode() + " : "
                                        + et.getExtendedErrorInformation().getErrorMessage() + " for flow "
                                        + flow.getIdentificativoFlusso());
                    } catch (StorageException | InvalidKeyException | URISyntaxException | JsonProcessingException es) {

                        this.logger.log(Level.SEVERE, () -> "[FlowsService]  Error " + es.getLocalizedMessage()
                                + " flow " + flow.getIdentificativoFlusso());
                    }
                });

            } catch (Exception e) {

                this.logger.severe(String.format("[FlowsService] Generic Error %s  in batch %s",
                        e.getLocalizedMessage(), partitionFlowsIndex));
            }

        });

        this.logger.log(Level.INFO, "[FlowsService] END flows storing ");
    }

    public void flowsBatchProcessing(List<TipoIdRendicontazione> partition, String idPA, int partitionFlowsIndex)
            throws InvalidKeyException, URISyntaxException, StorageException, JsonProcessingException {

        this.logger.log(Level.INFO, () -> "[FlowsService] flowsBatchProcessing - partition index: " + partitionFlowsIndex);

        CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.flowsQueue);
        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.flowsTable);

        TableBatchOperation batchOperation = new TableBatchOperation();

        partition.forEach(flow -> batchOperation.insert(
                new FlowEntity(flow.getIdentificativoFlusso(), flow.getDataOraFlusso().toString(), idPA)
        ));

        this.logger.log(Level.INFO, () -> "[FlowsService] Storing batch - partition index: " + partitionFlowsIndex);
        table.execute(batchOperation);

        FlowsMessage flows = new FlowsMessage();
        flows.setFlows(partition.toArray(TipoIdRendicontazione[]::new));
        flows.setIdPA(idPA);
        flows.setRetry(0);
        String message = new ObjectMapper().writeValueAsString(flows);

        this.logger.log(Level.INFO, () -> "[FlowsService] Sending messages - partition index: " + partitionFlowsIndex);
        queue.addMessage(new CloudQueueMessage(message));
    }

    public void flowProcessing(TipoIdRendicontazione flow, String idPA)
            throws InvalidKeyException, URISyntaxException, StorageException, JsonProcessingException {

        CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.flowsQueue);
        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.flowsTable);

        this.logger.log(Level.INFO, () -> "[FlowsService] Storing flow " + flow.getIdentificativoFlusso());
        table.execute(TableOperation.insert(new FlowEntity(flow.getIdentificativoFlusso(),
                flow.getDataOraFlusso().toString(), idPA)));

        FlowsMessage flows = new FlowsMessage();
        flows.setFlows(new TipoIdRendicontazione[] { flow });
        flows.setIdPA(idPA);
        flows.setRetry(Integer.valueOf(0));
        String message = new ObjectMapper().writeValueAsString(flows);

        this.logger.log(Level.INFO, () -> "[FlowsService] Sending messages:  " + message);
        queue.addMessage(new CloudQueueMessage(message));
    }

    private void createEnv() {
        AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, flowsTable, flowsQueue);
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
