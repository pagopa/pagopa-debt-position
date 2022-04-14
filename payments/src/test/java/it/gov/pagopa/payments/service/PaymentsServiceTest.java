package it.gov.pagopa.payments.service;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.spy;

import org.junit.ClassRule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.RetryNoRetry;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.CloudTableClient;
import com.microsoft.azure.storage.table.TableBatchOperation;
import com.microsoft.azure.storage.table.TableRequestOptions;

import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.exception.AppException;
import it.gov.pagopa.payments.model.PaymentsResultSegment;

@Testcontainers
@ExtendWith(MockitoExtension.class)
class PaymentsServiceTest {
		
	@ClassRule @Container
	public static GenericContainer<?> azurite =
	      new GenericContainer<>(
	              DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest"))
	          .withExposedPorts(10001, 10002, 10000);


    String storageConnectionString =
  	      String.format(
  	          "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;TableEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1;BlobEndpoint=http://%s:%s/devstoreaccount1",
  	          azurite.getContainerIpAddress(),
  	          azurite.getMappedPort(10002),
  	          azurite.getContainerIpAddress(),
  	          azurite.getMappedPort(10001),
  	          azurite.getContainerIpAddress(),
  	          azurite.getMappedPort(10000));
    
    CloudTable table = null;

    
    @BeforeEach
	void setUp() throws StorageException {
    	try {
        	CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
        	CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
        	TableRequestOptions tableRequestOptions = new TableRequestOptions();
        	tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance());
        	cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
        	table = cloudTableClient.getTableReference("receiptsTable");
        	table.createIfNotExists();
        } catch (Exception e) {
        	e.printStackTrace();
        }
    	TableBatchOperation batchOperation = new TableBatchOperation();
    	for (int i=0; i<10; i++) {
    		ReceiptEntity receiptEntity = new ReceiptEntity("org123456","iuv"+i);
    		receiptEntity.setDebtor("debtor"+i);
    		receiptEntity.setDocument("XML"+i);
	        batchOperation.insertOrReplace(receiptEntity);   
    	}
    	table.execute(batchOperation);
    	
	}
    
    @AfterEach
    void teardown() {
        if (null != table) {
        	try {
        		table.deleteIfExists();
        	} catch (Exception e) {
            	e.printStackTrace();
            }
        }
    }

	/**
	 *  GET RECEIPT BY IUV
	 */
	@Test
	void getReceiptByOrganizationFCAndIUV() throws Exception {
		
		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable"));
		
		ReceiptEntity re = paymentsService.getReceiptByOrganizationFCAndIUV("org123456", "iuv0");
		assertEquals("org123456", re.getPartitionKey());
		assertEquals("iuv0", re.getRowKey());
		assertEquals("debtor0", re.getDebtor());
	}
	
	@Test
	void getReceiptByOrganizationFCAndIUV_404() throws Exception {
		
		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable"));
		
		try {
			paymentsService.getReceiptByOrganizationFCAndIUV("org123456", "iuvx");
		} catch(AppException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getHttpStatus());
		}
	}
	
	/**
	 *  GET RECEIPTS
	 */
	@Test
	void getOrganizationReceipts_noFilter() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable"));
		
		PaymentsResultSegment<ReceiptEntity> res = paymentsService.getOrganizationReceipts(null, null, "org123456", null);
		assertNotNull(res);
		assertEquals(10, res.getResults().size());
		
	}
	
	@Test
	void getOrganizationReceipts_page_and_limit_filters() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable"));
		
		PaymentsResultSegment<ReceiptEntity> res = paymentsService.getOrganizationReceipts(5, 0, "org123456", null);
		assertNotNull(res);
		assertEquals(5, res.getResults().size());
		assertEquals(0, res.getCurrentPageNumber());
		assertEquals(true, res.isHasMoreResults());
		
	}
	
	@Test
	void getOrganizationReceipts_debtor_filter() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable"));
		
		PaymentsResultSegment<ReceiptEntity> res = paymentsService.getOrganizationReceipts(null, null, "org123456", "debtor5");
		assertNotNull(res);
		assertEquals(1, res.getResults().size());
		assertEquals(0, res.getCurrentPageNumber());
		
	}
	
	@Test
	void getOrganizationReceipts_all_filters() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable"));
		
		PaymentsResultSegment<ReceiptEntity> res = paymentsService.getOrganizationReceipts(5, 0, "org123456", "debtor5");
		assertNotNull(res);
		assertEquals(1, res.getResults().size());
		assertEquals(0, res.getCurrentPageNumber());
		
	}
	
	@Test
	void getOrganizationReceipts_404_page_not_exist() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable"));
		
		try {
			paymentsService.getOrganizationReceipts(5, 3, "org123456", null);
		} catch(AppException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getHttpStatus());
		}
		
	}
	
	@Test
	void getOrganizationReceipts_debtor_not_exist() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable"));
		
		PaymentsResultSegment<ReceiptEntity> res = paymentsService.getOrganizationReceipts(null, null, "org123456", "debtor15");
		assertNotNull(res);
		assertEquals(0, res.getResults().size());
		
	}
	
	

	
	
}