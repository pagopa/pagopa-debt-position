package it.gov.pagopa.payments.service;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.ClassRule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
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
import it.gov.pagopa.payments.entity.Status;
import it.gov.pagopa.payments.exception.AppException;
import it.gov.pagopa.payments.mock.MockUtil;
import it.gov.pagopa.payments.model.PaymentsModelResponse;
import it.gov.pagopa.payments.model.PaymentsResult;

@Testcontainers
@ExtendWith(MockitoExtension.class)
class PaymentsServiceTest {
	
	@Mock
    private GpdClient gpdClient;
		
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
    		receiptEntity.setStatus(Status.PAID.name());
	        batchOperation.insertOrReplace(receiptEntity);   
    	}
    	for (int i=10; i<15; i++) {
    		ReceiptEntity receiptEntity = new ReceiptEntity("org123456","iuv"+i);
    		receiptEntity.setDebtor("debtor"+i);
    		receiptEntity.setDocument("XML"+i);
    		receiptEntity.setStatus(Status.CREATED.name());
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
		
		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		ReceiptEntity re = paymentsService.getReceiptByOrganizationFCAndIUV("org123456", "iuv0");
		assertEquals("org123456", re.getPartitionKey());
		assertEquals("iuv0", re.getRowKey());
		assertEquals("debtor0", re.getDebtor());
	}
	
	@Test
	void getReceiptByOrganizationFCAndIUV_404() throws Exception {
		
		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		try {
			paymentsService.getReceiptByOrganizationFCAndIUV("org123456", "iuvx");
		} catch(AppException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getHttpStatus());
		}
	}
	
	@Test
	void getReceiptByOrganizationFCAndIUV_500() throws Exception {

		String wrongStorageConnectionString = "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;TableEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1;BlobEndpoint=http://%s:%s/devstoreaccount1";
		var paymentsService = spy(new PaymentsService(wrongStorageConnectionString, "receiptsTable", gpdClient));
		
		try {
			paymentsService.getReceiptByOrganizationFCAndIUV("org123456", "iuv0");
		} catch(AppException e) {
			assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, e.getHttpStatus());
		}
		
	}
	
	/**
	 *  GET RECEIPTS
	 */
	@Test
	void getOrganizationReceipts_noFilter() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		
		PaymentsResult<ReceiptEntity> res = paymentsService.getOrganizationReceipts(null, null, "org123456", null, null);
		assertNotNull(res);
		assertEquals(15, res.getResults().size());
		
	}
	
	@Test
	void getOrganizationReceipts_page_and_limit_filters() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		
		PaymentsResult<ReceiptEntity> res = paymentsService.getOrganizationReceipts(5, 0, "org123456", null, null);
		assertNotNull(res);
		assertEquals(5, res.getResults().size());
		assertEquals(0, res.getCurrentPageNumber());
		assertEquals(true, res.isHasMoreResults());
		
	}
	
	@Test
	void getOrganizationReceipts_debtor_filter() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		PaymentsResult<ReceiptEntity> res = paymentsService.getOrganizationReceipts(null, null, "org123456", "debtor5", null);
		assertNotNull(res);
		assertEquals(1, res.getResults().size());
		assertEquals(0, res.getCurrentPageNumber());
		
	}
	
	@Test
	void getOrganizationReceipts_PAID_service_filter() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		PaymentsResult<ReceiptEntity> res = paymentsService.getOrganizationReceipts(null, null, "org123456", null, "iuv5");
		assertNotNull(res);
		assertEquals(1, res.getResults().size());
		assertEquals(0, res.getCurrentPageNumber());
		
	}
	
	@Test
	void getOrganizationReceipts_CREATED_PO_PAID_service_filter() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		// precondition
		PaymentsModelResponse paymentModel = MockUtil.readModelFromFile("gpd/getPaymentOption.json", PaymentsModelResponse.class);
		when(gpdClient.getPaymentOption(anyString(), anyString())).thenReturn(paymentModel);
		
		PaymentsResult<ReceiptEntity> res = paymentsService.getOrganizationReceipts(null, null, "org123456", null, "iuv11");
		assertNotNull(res);
		assertEquals(1, res.getResults().size());
		assertEquals(0, res.getCurrentPageNumber());
		
	}
	
	@Test
	void getOrganizationReceipts_CREATED_PO_UNPAID_service_filter() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		// precondition
		PaymentsModelResponse paymentModel = MockUtil.readModelFromFile("gpd/getPaymentOption_PO_UNPAID.json", PaymentsModelResponse.class);
		when(gpdClient.getPaymentOption(anyString(), anyString())).thenReturn(paymentModel);
		
		PaymentsResult<ReceiptEntity> res = paymentsService.getOrganizationReceipts(null, null, "org123456", null, "iuv13");
		assertNotNull(res);
		assertEquals(0, res.getResults().size());
		assertEquals(0, res.getCurrentPageNumber());
		
	}
	
	@Test
	void getOrganizationReceipts_all_filters() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		
		PaymentsResult<ReceiptEntity> res = paymentsService.getOrganizationReceipts(5, 0, "org123456", "debtor5", "iuv5");
		assertNotNull(res);
		assertEquals(1, res.getResults().size());
		assertEquals(0, res.getCurrentPageNumber());
		
	}
	
	@Test
	void getOrganizationReceipts_404_page_not_exist() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		try {
			paymentsService.getOrganizationReceipts(5, 3, "org123456", null, null);
		} catch(AppException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getHttpStatus());
		}
		
	}
	
	@Test
	void getOrganizationReceipts_debtor_not_exist() throws Exception {

		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		PaymentsResult<ReceiptEntity> res = paymentsService.getOrganizationReceipts(null, null, "org123456", "debtor15", null);
		assertNotNull(res);
		assertEquals(0, res.getResults().size());
		
	}
	
	@Test
	void getOrganizationReceipts_500() throws Exception {

		String wrongStorageConnectionString = "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;TableEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1;BlobEndpoint=http://%s:%s/devstoreaccount1";
		var paymentsService = spy(new PaymentsService(wrongStorageConnectionString, "receiptsTable", gpdClient));
		
		try {
			paymentsService.getOrganizationReceipts(null, null, "org123456", null, null);
		} catch(AppException e) {
			assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, e.getHttpStatus());
		}
		
	}
	
	/**
	 *  GPD CHECK
	 */
	@Test
	void getGPDCheckedReceiptsList() throws Exception {
		
		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		List<ReceiptEntity> receipts = new ArrayList<>();
		ReceiptEntity re1 = new ReceiptEntity("111", "aaa");
		re1.setStatus(Status.PAID.name());
		ReceiptEntity re2 = new ReceiptEntity("222", "bbb");
		re2.setStatus(Status.PAID.name());
		ReceiptEntity re3 = new ReceiptEntity("333", "ccc");
		re3.setStatus(Status.PAID.name());
		receipts.add(re1);
		receipts.add(re2);
		receipts.add(re3);
		PaymentsResult<ReceiptEntity> mock = new PaymentsResult<>();
		mock.setCurrentPageNumber(0);
		mock.setHasMoreResults(false);
		mock.setPageSize(5);
		mock.setLength(receipts.size());
		mock.setResults(receipts);
		
		
		
		PaymentsResult<ReceiptEntity> result = paymentsService.getGPDCheckedReceiptsList(table, mock);
		
		assertEquals(mock.getLength(), result.getLength());
		assertEquals(mock.getResults().size(), result.getResults().size());
	}
	
	
	@Test
	void getGPDCheckedReceiptsList_GPDCheckFail() throws Exception {
		
		var paymentsService = spy(new PaymentsService(storageConnectionString, "receiptsTable", gpdClient));
		
		List<ReceiptEntity> receipts = new ArrayList<>();
		ReceiptEntity re1 = new ReceiptEntity("111", "aaa");
		ReceiptEntity re2 = new ReceiptEntity("222", "bbb");
		ReceiptEntity re3 = new ReceiptEntity("333", "ccc");
		receipts.add(re1);
		receipts.add(re2);
		receipts.add(re3);
		PaymentsResult<ReceiptEntity> mock = new PaymentsResult<>();
		mock.setCurrentPageNumber(0);
		mock.setHasMoreResults(false);
		mock.setPageSize(5);
		mock.setLength(receipts.size());
		mock.setResults(receipts);
		
		// GPD risponde sempre UNPAID
		PaymentsModelResponse paymentModel = MockUtil.readModelFromFile("gpd/getPaymentOption_PO_UNPAID.json", PaymentsModelResponse.class);
        when(gpdClient.getPaymentOption(anyString(), anyString())).thenReturn(paymentModel);
		
		PaymentsResult<ReceiptEntity> result = paymentsService.getGPDCheckedReceiptsList(table, mock);
		
		// tutte le ricevute sono state scartate
		assertEquals(0, result.getLength());
		assertEquals(0, result.getResults().size());
	}
	
	

	
	
}