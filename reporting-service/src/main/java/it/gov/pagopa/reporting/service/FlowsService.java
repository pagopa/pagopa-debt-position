package it.gov.pagopa.hubpa.service;

import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
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

import it.gov.pagopa.hubpa.servicewsdl.FaultBean;
import it.gov.pagopa.hubpa.servicewsdl.TipoIdRendicontazione;

import java.io.IOException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

public class FlowsService {

    private String storageConnectionString;
    private String identificativoIntemediarioPA;
    private String identificativoStazioneIntermediarioPA;
    private String nodePassword;
    private String cert;
    private String key;
    private String certPassword;
    private String containerBlob;
    private Logger logger;

    public FlowsService(String storageConnectionString, String identificativoIntemediarioPA,
            String identificativoStazioneIntermediarioPA, String nodePassword, String cert, String key,
            String certPassword, String containerBlob, Logger logger) {

        this.storageConnectionString = storageConnectionString;
        this.identificativoIntemediarioPA = identificativoIntemediarioPA;
        this.identificativoStazioneIntermediarioPA = identificativoStazioneIntermediarioPA;
        this.nodePassword = nodePassword;
        this.cert = cert;
        this.key = key;
        this.certPassword = certPassword;
        this.containerBlob = containerBlob;
        this.logger = logger;
    }

    public void flowsXmlDownloading(List<TipoIdRendicontazione> flows, String idPA) {

        this.logger.log(Level.INFO, "[FlowsDownloadFunction] START flows downloading ");

        try {
            NodeService nodeService = this.getNodeServiceInstance();

            nodeService.initSslConfiguration();

            IntStream.range(0, flows.size()).forEach(index -> {

                nodeService.callNodoChiediElencoFlussiRendicontazione(idPA, flows.get(index).getIdentificativoFlusso());

                FaultBean faultBean = nodeService.getNodoChiediFlussoRendicontazioneFault();
                DataHandler xmlReporting = nodeService.getNodoChiediElencoFlussiRendicontazioneXmlReporting();

                if (faultBean != null) {

                    logger.log(Level.INFO,
                            () -> "[FlowsDownloadFunction] faultBean DESC " + faultBean.getDescription());
                } else if (xmlReporting != null) {

                    BlobServiceClient blobServiceClient = new BlobServiceClientBuilder()
                            .connectionString(this.storageConnectionString).buildClient();

                    BlobContainerClient containerClient = blobServiceClient.getBlobContainerClient(this.containerBlob);

                    BlobClient blobClient = containerClient.getBlobClient(flows.get(index).getIdentificativoFlusso()
                            + "##" + flows.get(index).getDataOraFlusso().toString() + ".xml");

                    logger.log(Level.INFO, () -> "[FlowsDownloadFunction] Uploading " + blobClient.getBlobUrl() + " in "
                            + this.containerBlob);

                    try {

                        blobClient.upload(BinaryData.fromStream(xmlReporting.getInputStream()));
                    } catch (IOException e) {
                        logger.log(Level.SEVERE, () -> "[FlowsDownloadFunction] Upload failed "
                                + blobClient.getBlobUrl() + " in " + this.containerBlob);
                    }

                    logger.log(Level.INFO, () -> "[FlowsDownloadFunction] Uploaded " + blobClient.getBlobUrl() + " in "
                            + this.containerBlob);
                }
            });

        } catch (UnrecoverableKeyException | CertificateException | NoSuchAlgorithmException | KeyStoreException
                | InvalidKeySpecException | KeyManagementException e) {

            logger.log(Level.SEVERE, () -> "[FlowsDownloadFunction] Configuration Error " + e.getLocalizedMessage());
        } catch (Exception e) {

            logger.log(Level.SEVERE, () -> "[FlowsDownloadFunction] Generic Error " + e.getMessage());
        }

        this.logger.log(Level.INFO, "[FlowsService] END flows storing ");
    }

    public NodeService getNodeServiceInstance() {

        return new NodeService(this.identificativoIntemediarioPA, this.identificativoStazioneIntermediarioPA,
                this.nodePassword, this.cert, this.key, this.certPassword);
    }
}
