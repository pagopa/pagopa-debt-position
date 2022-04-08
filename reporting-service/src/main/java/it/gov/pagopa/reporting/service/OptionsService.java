package it.gov.pagopa.reporting.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueue;
import com.microsoft.azure.storage.queue.CloudQueueMessage;
import it.gov.pagopa.reporting.models.OptionsMessage;
import it.gov.pagopa.reporting.models.PaymentOption;
import it.gov.pagopa.reporting.util.AzuriteStorageUtil;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class OptionsService {

    private final String storageConnectionString;
    private final String optionsQueue;
    private final Logger logger;
    private final int optionsForMessage = 2;

    private final String delay = System.getenv("DELAY_ATTEMPS");
    private final String timeToLiveInSeconds = System.getenv("QUEUE_RETENTION_SEC");


    public OptionsService(String storageConnectionString, String optionsQueue, Logger logger) {

        this.storageConnectionString = storageConnectionString;
        this.optionsQueue = optionsQueue;
        this.logger = logger;
    }

    public void optionsProcessing(List<PaymentOption> options, String idPA, String idFlow, String dataFlow) throws JsonProcessingException {

        // try to create blob container
        AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, null, optionsQueue, null);
        try {
            azuriteStorageUtil.createQueue();
        } catch (URISyntaxException | InvalidKeyException | StorageException e) {
            this.logger.severe(String.format("[AzureStorage] Problem to create queue: %s", e.getMessage()));
        }

        // step 11

        this.logger.log(Level.INFO, "[OptionsService] START options queue ");

        List<List<PaymentOption>> partitionOptions = Lists.partition(options, optionsForMessage);

        OptionsMessage optionsMsg;
        List<String> messages = new ArrayList<>();
        for (List<PaymentOption> partitionOption : partitionOptions) {
            optionsMsg = new OptionsMessage();
            optionsMsg.setFlowDate(dataFlow);
            optionsMsg.setIdPA(idPA);
            optionsMsg.setIdFlow(idFlow);
            optionsMsg.setPaymentOptions(partitionOption);
            optionsMsg.setRetryCount(0);
            messages.add(new ObjectMapper().writeValueAsString(optionsMsg));
        }

        this.logger.log(Level.INFO, () -> "[OptionsService] " + options.size() + " flows in " + partitionOptions.size()
                + "  batch of size " + optionsForMessage);

        try {
            CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                    .getQueueReference(this.optionsQueue);
            queue.createIfNotExists();
            this.logger.log(Level.INFO, () -> "[OptionsService] Sending messages ");

            messages.forEach(msg -> {
                try {
                    this.logger.log(Level.INFO, () -> "[OptionsService] sent message " + msg);
                    queue.addMessage(new CloudQueueMessage(msg));
                } catch (StorageException e) {
                    logger.log(Level.INFO, () -> "[OptionsService] sent exception : " + e.getMessage());
                }
            });

        } catch (URISyntaxException | StorageException | InvalidKeyException e) {
            this.logger.log(Level.INFO, () -> "[OptionsService] queue exception : " + e.getMessage());
        }

        this.logger.log(Level.INFO, "[OptionsService] END options queue ");
    }

    public void insertMessage(OptionsMessage msg) {

        try {
            logger.log(Level.INFO, () -> "[OptionsService] pushing debt position in queue [" + optionsQueue + "]: " + msg);

            AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, null, optionsQueue, null);
            azuriteStorageUtil.createQueue();

            CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).
                    createCloudQueueClient()
                    .getQueueReference(optionsQueue);

            int timeToLive = timeToLiveInSeconds != null ? Integer.parseInt(timeToLiveInSeconds) : 60;
            int initialVisibilityDelay = delay != null ? Integer.parseInt(delay) : 0;

            queue.addMessage(new CloudQueueMessage(new ObjectMapper().writeValueAsString(msg)), timeToLive, initialVisibilityDelay, null, null);
        } catch (URISyntaxException | StorageException | InvalidKeyException | JsonProcessingException e) {
            this.logger.log(Level.SEVERE, () -> "[OptionsService ERROR] Error " + e);
        }
    }

}
