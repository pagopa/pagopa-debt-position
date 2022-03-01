package it.gov.pagopa.reporting.service;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.IntStream;

import javax.activation.DataHandler;

import com.azure.core.util.BinaryData;
import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;

import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;

import java.io.IOException;

public class FlowsService {

    private String storageConnectionString;
    private String identificativoIntemediarioPA;
    private String identificativoStazioneIntermediarioPA;
    private String paaPassword;
    private String containerBlob;
    private Logger logger;

    public FlowsService(String storageConnectionString, String identificativoIntemediarioPA,
                        String identificativoStazioneIntermediarioPA, String paaPassword, String containerBlob, Logger logger) {

        this.storageConnectionString = storageConnectionString;
        this.identificativoIntemediarioPA = identificativoIntemediarioPA;
        this.identificativoStazioneIntermediarioPA = identificativoStazioneIntermediarioPA;
        this.paaPassword = paaPassword;
        this.containerBlob = containerBlob;
        this.logger = logger;
    }

    public void flowsXmlDownloading(List<TipoIdRendicontazione> flows, String idPA) {

        this.logger.log(Level.INFO, "[RetrieveDetails/FlowsService] START flows downloading ");

        try {
            NodeService nodeService = this.getNodeServiceInstance();

            IntStream.range(0, flows.size()).forEach(index -> {

                // nodoChiediFlussoRendicontazione(idPA, idFlow)
                // https://github.com/pagopa/pagopa-api/blob/master/nodo/NodoPerPa.wsdl#L523
                nodeService.callNodoChiediFlussoRendicontazione(idPA, flows.get(index).getIdentificativoFlusso());

                FaultBean faultBean = nodeService.getNodoChiediFlussoRendicontazioneFault();
                DataHandler xmlReporting = nodeService.getNodoChiediElencoFlussiRendicontazioneXmlReporting();

                if (faultBean != null) {
                    logger.log(Level.INFO,
                            () -> "[RetrieveDetails/FlowsService] faultBean DESC " + faultBean.getDescription());
                    // TODO to analyze what should be done
                } else if (xmlReporting != null) {

                    BlobServiceClient blobServiceClient = new BlobServiceClientBuilder()
                            .connectionString(this.storageConnectionString).buildClient();

                    BlobContainerClient flowsContainerClient = blobServiceClient.getBlobContainerClient(this.containerBlob);

                    // dataOra##idPa##idflow.xml
                    // added split to remove millis from name
                    BlobClient blobClient = flowsContainerClient.getBlobClient(
                            flows.get(index).getDataOraFlusso().toString().split("\\.")[0] + "##" + idPA + "##" + flows.get(index).getIdentificativoFlusso() + ".xml"
                    );

                    logger.log(Level.INFO, () ->
                            "[RetrieveDetails/FlowsService] Uploading " + blobClient.getBlobUrl() + " in " + this.containerBlob);

                    try {
                        // save base64 as xml string
                        blobClient.upload(BinaryData.fromStream(xmlReporting.getInputStream()));
                    } catch (IOException e) {
                        logger.log(Level.SEVERE, () -> "[RetrieveDetails/FlowsService] Upload failed "
                                + blobClient.getBlobUrl() + " in " + this.containerBlob);
                    }

                    logger.log(Level.INFO, () ->
                            "[RetrieveDetails/FlowsService] Uploaded " + blobClient.getBlobUrl() + " in " + this.containerBlob);
                }
            });

        } catch (Exception e) {
            logger.log(Level.SEVERE, () -> "[RetrieveDetails/FlowsService] Generic Error " + e.getMessage());
        }

        this.logger.log(Level.INFO, "[RetrieveDetails/FlowsService] END flows storing ");
    }

    public NodeService getNodeServiceInstance() {

        return new NodeService(this.identificativoIntemediarioPA, this.identificativoStazioneIntermediarioPA,
                this.paaPassword);
    }
}
