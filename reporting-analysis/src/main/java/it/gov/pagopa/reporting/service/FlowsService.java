package it.gov.pagopa.reporting.service;

import java.io.ByteArrayOutputStream;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.blob.models.BlobStorageException;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.table.*;
import it.gov.pagopa.reporting.entity.FlowEntity;
import it.gov.pagopa.reporting.model.Flow;
import it.gov.pagopa.reporting.util.AzuriteStorageUtil;
import it.gov.pagopa.reporting.util.FlowConverter;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;

public class FlowsService {

    private String storageConnectionString;
    private String flowsTable;
    private String containerBlob;
    private Logger logger;

    public FlowsService(String storageConnectionString, String flowsTable, String containerBlob, Logger logger) {

        this.storageConnectionString = storageConnectionString;
        this.flowsTable = flowsTable;
        this.containerBlob = containerBlob;
        this.logger = logger;
    }

    public List<Flow> getByOrganization(String organizationId) throws URISyntaxException, InvalidKeyException, StorageException, RuntimeException {
        logger.log(Level.INFO, () -> String.format("[FlowsService] START get by organization: %s", organizationId));

        // try to create table
        AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, flowsTable, null);
        azuriteStorageUtil.createTable();

        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.flowsTable);

        TableQuery<FlowEntity> query = TableQuery.from(FlowEntity.class).where(
                TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, organizationId)
        );

        Iterable<FlowEntity> result = table.execute(query);

        List<FlowEntity> flowList = new ArrayList<>();
        result.forEach(flowList::add);

        Converter<FlowEntity, Flow> converter = new FlowConverter();
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.createTypeMap(FlowEntity.class, Flow.class).setConverter(converter);

        return flowList.stream().map(flow -> modelMapper.map(flow, Flow.class)).collect(Collectors.toList());
    }

    public String getByFlow(String organizationId, String flowId, String flowDate) throws BlobStorageException {
        logger.log(Level.INFO, () -> String.format("[FlowsService] START get by flow: %s - %s - %s", organizationId, flowId, flowDate));

        // try to create blob container
        AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, null, containerBlob);
        try {
            azuriteStorageUtil.createTable();
        } catch (Exception e) {
            this.logger.severe(String.format("[AzureStorage] Problem to create table: %s", e.getMessage()));
        }

        BlobServiceClient blobServiceClient = new BlobServiceClientBuilder()
                .connectionString(this.storageConnectionString).buildClient();

        BlobContainerClient flowsContainerClient = blobServiceClient.getBlobContainerClient(this.containerBlob);

        // dataOra##idPa##idflow.xml
        BlobClient blobClient = flowsContainerClient.getBlobClient(
                flowDate + "##" + organizationId + "##" + flowId + ".xml"
        );

        //creating an object of output stream to receive the file's content from azure blob.
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        blobClient.download(outputStream);

        return outputStream.toString();
    }

}
