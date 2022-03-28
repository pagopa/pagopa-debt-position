package it.gov.pagopa.payments.utils;


import java.net.URISyntaxException;
import java.security.InvalidKeyException;

import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.OperationContext;
import com.microsoft.azure.storage.RetryNoRetry;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.core.Logger;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.CloudTableClient;
import com.microsoft.azure.storage.table.TableRequestOptions;

public class AzuriteStorageUtil {

    private boolean debugAzurite = Boolean.parseBoolean(System.getenv("DEBUG_AZURITE"));

    private String storageConnectionString;
    
    public AzuriteStorageUtil(String storageConnectionString) {
    	this.storageConnectionString = storageConnectionString;
    	
    }
    
    public AzuriteStorageUtil(String storageConnectionString, boolean debugAzurite) {
    	this.storageConnectionString = storageConnectionString;
    	this.debugAzurite = debugAzurite;
    }

    // Create a new table
    public void createTable(String tableName) throws URISyntaxException, StorageException, InvalidKeyException {
        if (debugAzurite) {
            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
            TableRequestOptions tableRequestOptions = new TableRequestOptions();
            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance()); // disable retry to complete faster
            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
            CloudTable table = cloudTableClient.getTableReference(tableName);
            try {
            	table.create();
            } catch (Exception e) {
            	Logger.info(new OperationContext(), "Table already exist:" + tableName);
            }
        }
    }
}
