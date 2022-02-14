package it.gov.pagopa.hubpa.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueue;
import com.microsoft.azure.storage.queue.CloudQueueMessage;

import it.gov.pagopa.hubpa.models.BooleanResponseModel;
import it.gov.pagopa.hubpa.models.OptionsMessage;
import it.gov.pagopa.hubpa.models.OptionsReportingModel;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;

public class OptionsService {

    private String storageConnectionString;
    private String optionsQueue;
    private String paymentHost;
    private String auxDigit;
    private Logger logger;
    private int optionsForMessage = 2;

    public OptionsService(String storageConnectionString, String optionsQueue, String paymentHost, String auxDigit,
            Logger logger) {

        this.storageConnectionString = storageConnectionString;
        this.optionsQueue = optionsQueue;
        this.paymentHost = paymentHost;
        this.auxDigit = auxDigit;
        this.logger = logger;
    }

    public void optionsProcessing(List<String> options, String idFlow, String dataFlow) throws JsonProcessingException {

        this.logger.log(Level.INFO, "[OptionsService] START options queue ");

        List<List<String>> partitionOptions = Lists.partition(options, optionsForMessage);

        OptionsMessage optionsMsg;
        List<String> messages = new ArrayList<>();
        for (List<String> partitionOption : partitionOptions) {
            optionsMsg = new OptionsMessage();
            optionsMsg.setDateFlow(dataFlow);
            optionsMsg.setIdFlow(idFlow);
            optionsMsg.setIuvs(partitionOption.stream().toArray(String[]::new));
            messages.add(new ObjectMapper().writeValueAsString(optionsMsg));
        }

        this.logger.log(Level.INFO, () -> "[OptionsService] " + options.size() + " flows in " + partitionOptions.size()
                + "  batch of size " + optionsForMessage);

        try {
            CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                    .getQueueReference(this.optionsQueue);
            queue.createIfNotExists();
            this.logger.log(Level.INFO, () -> "[OptionsService] Sending messages ");

            messages.stream().forEach(msg -> {
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

    public void callPaymentServiceToReportOption(OptionsMessage options) {

        this.logger.log(Level.INFO, () -> "[OptionsService]  call PaymentService to report the options related to flow "
                + options.getIdFlow());

        BooleanResponseModel response = ClientBuilder.newClient()
                .target(this.paymentHost + "/payments/options/reporting").request()
                .post(Entity.entity(this.getOptionsReportingModel(options), MediaType.APPLICATION_JSON),
                        BooleanResponseModel.class);

        if (Boolean.FALSE.equals(response.getResult())) {

            throw new IllegalArgumentException("Option reporting error for flow " + options.getIdFlow());
        }

        this.logger.log(Level.INFO, "[OptionsService] options reported");
    }

    public OptionsReportingModel getOptionsReportingModel(OptionsMessage optionsMessage) {

        List<String> notificationCodes = Arrays.asList(optionsMessage.getIuvs()).stream()
                .map(iuv -> this.auxDigit + iuv).collect(Collectors.toList());

        OptionsReportingModel optionsReportingRequest = new OptionsReportingModel();
        optionsReportingRequest.setIdFlow(optionsMessage.getIdFlow());
        optionsReportingRequest.setDateFlow(optionsMessage.getDateFlow());
        optionsReportingRequest.setNotificationCodes(notificationCodes);

        return optionsReportingRequest;
    }
}
