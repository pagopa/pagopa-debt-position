package it.gov.pagopa.reporting.service;

import com.azure.core.util.BinaryData;
import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueue;
import com.microsoft.azure.storage.queue.CloudQueueMessage;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.TableOperation;
import com.sun.xml.ws.client.ClientTransportException;
import it.gov.pagopa.reporting.entities.FlowEntity;
import it.gov.pagopa.reporting.models.FlowsMessage;
import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;
import it.gov.pagopa.reporting.util.AzuriteStorageUtil;

import javax.activation.DataHandler;
import java.io.IOException;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.IntStream;

public class FlowsService {

    private final String storageConnectionString;
    private final String identificativoIntemediarioPA;
    private final String identificativoStazioneIntermediarioPA;
    private final String paaPassword;
    private final String containerBlob;
    private final String flowsQueue;
    private final String flowsTable;
    private final int timeToLiveInSeconds;
    private final int initialVisibilityDelayInSeconds;
    private final int maxRetryQueuing;
    private final Logger logger;

    public FlowsService(String storageConnectionString, String identificativoIntemediarioPA,
                        String identificativoStazioneIntermediarioPA, String paaPassword, String containerBlob, String flowsQueue,
                        String flowsTable, int maxRetryQueuing, int timeToLiveInSeconds, int initialVisibilityDelayInSeconds, Logger logger) {

        this.storageConnectionString = storageConnectionString;
        this.identificativoIntemediarioPA = identificativoIntemediarioPA;
        this.identificativoStazioneIntermediarioPA = identificativoStazioneIntermediarioPA;
        this.paaPassword = paaPassword;
        this.containerBlob = containerBlob;
        this.flowsQueue = flowsQueue;
        this.flowsTable = flowsTable;
        this.timeToLiveInSeconds = timeToLiveInSeconds;
        this.initialVisibilityDelayInSeconds = initialVisibilityDelayInSeconds;
        this.maxRetryQueuing = maxRetryQueuing;
        this.logger = logger;
    }

    public void flowsXmlDownloading(List<TipoIdRendicontazione> flows, String idPA, Integer retry) {

        this.logger.log(Level.INFO, "[RetrieveDetails/FlowsService] START flows downloading ");

        // try to create blob container
        AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, null, null, containerBlob);
        try {
            azuriteStorageUtil.createBlob();
        } catch (NullPointerException e) {
            this.logger.severe(String.format("[AzureStorage] Problem to create blob: %s", e.getMessage()));
        }

        NodeService nodeService = this.getNodeServiceInstance();

        IntStream.range(0, flows.size()).forEach(index -> {
            TipoIdRendicontazione flow = flows.get(index);
            try {
                // nodoChiediFlussoRendicontazione(idPA, idFlow)
                // https://github.com/pagopa/pagopa-api/blob/master/nodo/NodoPerPa.wsdl#L523
                nodeService.callNodoChiediFlussoRendicontazione(idPA, flow.getIdentificativoFlusso());

                FaultBean faultBean = nodeService.getNodoChiediFlussoRendicontazioneFault();
                DataHandler xmlReporting = nodeService.getNodoChiediElencoFlussiRendicontazioneXmlReporting();

                if (faultBean != null) {
                    logger.log(Level.SEVERE,
                            () -> "[RetrieveDetails/FlowsService] faultBean DESC " + faultBean.getDescription());
                    // TODO to analyze what should be done
                } else if (xmlReporting != null) {
                    saveBlob(flow, idPA, xmlReporting);

                    logger.log(Level.INFO, () ->
                            "[RetrieveDetails/FlowsService] Uploaded in " + this.containerBlob);
                }
            } catch (IOException e) {
                logger.log(Level.SEVERE, () -> "[RetrieveDetails/FlowsService] Upload failed in " + this.containerBlob);
            } catch (ClientTransportException e) {
                logger.log(Level.SEVERE, () -> "[NODO Connection down] " + idPA + " - " + flow.getIdentificativoFlusso());

                if (retry < maxRetryQueuing) {
                    try {
                        reQueuingMessage(idPA, flow, retry);
                    } catch (URISyntaxException | StorageException | InvalidKeyException | JsonProcessingException ex) {
                        logger.log(Level.SEVERE, () -> "[RetrieveDetails/FlowsService] Problem to re-queuing: " + idPA + " - " + flow.getIdentificativoFlusso());
                    }
                } else {
                    logger.log(Level.SEVERE, () -> "[NODO Connection down]  Max retry exceeded.");

                    try {
                        removeFlowFromFlowsTable(idPA, flow);
                    } catch (URISyntaxException | StorageException | InvalidKeyException ex) {
                        logger.log(Level.SEVERE, () -> "[RetrieveDetails/FlowsService] Problem to delete flow from FlowsTable: "
                                + idPA + " - " + flow.getIdentificativoFlusso());
                    }
                }
            }
        });

        this.logger.log(Level.INFO, "[RetrieveDetails/FlowsService] END flows storing ");
    }

    protected void saveBlob(TipoIdRendicontazione flow, String idPA, DataHandler xmlReporting) throws IOException {
        BlobServiceClient blobServiceClient = new BlobServiceClientBuilder()
                .connectionString(this.storageConnectionString).buildClient();

        BlobContainerClient flowsContainerClient = blobServiceClient.getBlobContainerClient(this.containerBlob);

        // dataOra##idPa##idflow.xml
        // added split to remove millis from name
        BlobClient blobClient = flowsContainerClient.getBlobClient(
                flow.getDataOraFlusso().toString().split("\\.")[0] + "##" + idPA + "##" + flow.getIdentificativoFlusso() + ".xml"
        );

        logger.log(Level.INFO, () ->
                "[RetrieveDetails/FlowsService] Uploading " + blobClient.getBlobUrl() + " in " + this.containerBlob);


        // save base64 as xml string
        blobClient.upload(BinaryData.fromStream(xmlReporting.getInputStream()));
    }

    protected void reQueuingMessage(String idPA, TipoIdRendicontazione flow, int retry) throws URISyntaxException, InvalidKeyException, StorageException, JsonProcessingException {
        CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                .getQueueReference(this.flowsQueue);

        FlowsMessage fm = new FlowsMessage();
        fm.setFlows(new TipoIdRendicontazione[]{flow});
        fm.setIdPA(idPA);
        fm.setRetry(retry);
        String message = new ObjectMapper().writeValueAsString(fm);
        queue.addMessage(new CloudQueueMessage(message), timeToLiveInSeconds, initialVisibilityDelayInSeconds, null, null);
    }

    protected void removeFlowFromFlowsTable(String idPA, TipoIdRendicontazione flow) throws URISyntaxException, InvalidKeyException, StorageException {
        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.flowsTable);

        FlowEntity flowEntity = new FlowEntity(flow.getIdentificativoFlusso(), flow.getDataOraFlusso().toString(), idPA);
        TableOperation operation = TableOperation.delete(flowEntity);
        table.execute(operation);
    }

    public NodeService getNodeServiceInstance() {
        return new NodeService(this.identificativoIntemediarioPA, this.identificativoStazioneIntermediarioPA,
                this.paaPassword);
    }
}
