package it.gov.pagopa.reporting.utils;


import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.RetryNoRetry;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueue;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.CloudTableClient;
import com.microsoft.azure.storage.table.TableRequestOptions;
import lombok.AllArgsConstructor;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;

@AllArgsConstructor
public class AzuriteStorageUtil {

    private final boolean debugAzurite = Boolean.parseBoolean(System.getenv("DEBUG_AZURITE"));

    private String storageConnectionString;
    private String flowsTable;
    private String flowsQueue;

    // Create a new table
    public void createTable() throws URISyntaxException, InvalidKeyException, StorageException, RuntimeException {
        if (debugAzurite) {
            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
            TableRequestOptions tableRequestOptions = new TableRequestOptions();
            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster
            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
            CloudTable table = cloudTableClient.getTableReference(flowsTable);
            table.createIfNotExists();
        }
    }

    // Create a new queue
    public void createQueue() throws URISyntaxException, InvalidKeyException, StorageException, RuntimeException {
        if (debugAzurite) {
            CloudQueue queue = CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient()
                    .getQueueReference(flowsQueue);
            queue.createIfNotExists();
        }
    }
}
