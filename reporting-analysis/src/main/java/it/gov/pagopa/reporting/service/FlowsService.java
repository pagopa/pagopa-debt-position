package it.gov.pagopa.reporting.service;

import com.azure.storage.blob.BlobClient;
import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.QueryTableOperation;
import com.microsoft.azure.storage.table.TableQuery;
import it.gov.pagopa.reporting.entity.FlowEntity;
import it.gov.pagopa.reporting.model.Flow;
import it.gov.pagopa.reporting.util.AzuriteStorageUtil;
import it.gov.pagopa.reporting.util.FlowConverter;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;

import java.io.ByteArrayOutputStream;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

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

    public List<Flow> getByOrganization(String organizationId, String flowDate) throws URISyntaxException, InvalidKeyException, StorageException, RuntimeException {
        logger.log(Level.INFO, () -> String.format("[FlowsService] START get by organization: %s", organizationId));

        // try to create table
        AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, flowsTable, null);
        azuriteStorageUtil.createTable();

        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.flowsTable);

        String queryWhereClause = TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, organizationId);
        /*
        * The saved flow date field is a string data, so it cannot be filtered as a numeric or temporal data. Also, in Azure Table storage it does not
        * exists the LIKE clause, so a workaround is made using ASCII character evaluation in 'ge' and 'le' operators. In particular, adding 'T0' string permits
        * to evaluate all dates that are greater or equals than HH=00 of passed date, and adding T3 string permits to evaluate all dates that are lower or equals than
        * HH=23 (because character '2' is lower than character '3' and over hour '23' is not a valid date)
        */
        if (flowDate != null) {
            String flowDateLowerLimitClause = TableQuery.generateFilterCondition("FlowDate", TableQuery.QueryComparisons.GREATER_THAN_OR_EQUAL, flowDate + "T0");
            String flowDateUpperLimitClause = TableQuery.generateFilterCondition("FlowDate", TableQuery.QueryComparisons.LESS_THAN_OR_EQUAL, flowDate + "T3");
            String flowDateIntervalLimitClause = TableQuery.combineFilters(flowDateLowerLimitClause, "and", flowDateUpperLimitClause);
            queryWhereClause = TableQuery.combineFilters(queryWhereClause, "and", flowDateIntervalLimitClause);
        }

        TableQuery<FlowEntity> query = TableQuery.from(FlowEntity.class).where(queryWhereClause);

        Iterable<FlowEntity> result = table.execute(query);

        List<FlowEntity> flowList = new ArrayList<>();
        result.forEach(flowList::add);

        Converter<FlowEntity, Flow> converter = new FlowConverter();
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.createTypeMap(FlowEntity.class, Flow.class).setConverter(converter);

        return flowList.stream().map(flow -> modelMapper.map(flow, Flow.class)).collect(Collectors.toList());
    }

    public String getByFlow(String organizationId, String flowId, String flowDate) throws Exception {
        logger.log(Level.INFO, () -> String.format("[FlowsService] START get by flow: %s - %s - %s", organizationId, flowId, flowDate));

        // try to create blob container
        AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, null, containerBlob);
        azuriteStorageUtil.createTable();

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
