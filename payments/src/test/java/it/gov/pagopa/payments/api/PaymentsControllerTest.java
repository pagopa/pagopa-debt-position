package it.gov.pagopa.payments.api;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

import java.util.ArrayList;

import org.junit.ClassRule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import com.microsoft.azure.storage.StorageException;

import it.gov.pagopa.payments.PaymentsApplication;
import it.gov.pagopa.payments.controller.receipt.impl.PaymentsController;
import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.exception.AppError;
import it.gov.pagopa.payments.exception.AppException;
import it.gov.pagopa.payments.model.PaymentsResultSegment;
import it.gov.pagopa.payments.model.ReceiptsInfo;
import it.gov.pagopa.payments.service.PaymentsService;

@SpringBootTest(classes = PaymentsApplication.class)
@AutoConfigureMockMvc
@Testcontainers
@ExtendWith(MockitoExtension.class)
class PaymentsControllerTest {

	@Autowired
	private MockMvc mvc;

	@Mock
	private ModelMapper modelMapperMock;
	
	@InjectMocks
	private PaymentsController paymentsController;
	
	@Mock
	private PaymentsService paymentsService;
	
	
	
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


    //@Autowired
    //private PaymentsService paymentsService = new PaymentsService(storageConnectionString, "receiptsTable");
    
    @BeforeEach
	void setUp() throws StorageException {
    	/*
    	CloudTable table = null;
    	try {
        	CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
        	CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
        	TableRequestOptions tableRequestOptions = new TableRequestOptions();
        	tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance());
        	cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
        	table = cloudTableClient.getTableReference("receiptsTable");
        	table.createIfNotExists();
        } catch (Exception e) {
        	log.info("Error during table creation", e);
        }
    	TableBatchOperation batchOperation = new TableBatchOperation();
    	for (int i=0; i<10; i++) {
    		ReceiptEntity receiptEntity = new ReceiptEntity("org123456","iuv"+i);
    		receiptEntity.setDebtor("debtor"+i);
    		receiptEntity.setDocument("XML"+i);
	        batchOperation.insertOrReplace(receiptEntity);   
    	}
    	table.execute(batchOperation);
    	*/   
	}

	/**
	 *  GET RECEIPT BY IUV
	 */
	@Test
	void getReceiptByIUV_200() throws Exception {
		//precondition
		ReceiptEntity receipt = new ReceiptEntity("mock", "mock");
		receipt.setDebtor("XML");
		when(paymentsService.getReceiptByOrganizationFCAndIUV(anyString(), anyString())).thenReturn(receipt);
		
		ResponseEntity<String> res = paymentsController.getReceiptByIUV(anyString(), anyString());
		assertEquals(HttpStatus.OK, res.getStatusCode());
	}
	
	@Test
	void getReceiptByIUV_404() throws Exception {
		//precondition
		doThrow(new AppException(AppError.RECEIPT_NOT_FOUND, "111", "222")).when(paymentsService).getReceiptByOrganizationFCAndIUV(anyString(), anyString());
		try {
			paymentsController.getReceiptByIUV(anyString(), anyString());
		} catch(AppException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getHttpStatus());
		}
		
	}
	
	/**
	 *  GET RECEIPTS
	 */
	@Test
	void getOrganizationReceipts_200() throws Exception {
		//precondition
		PaymentsResultSegment<ReceiptEntity> receipts = new PaymentsResultSegment<ReceiptEntity>();
		receipts.setResults(new ArrayList<ReceiptEntity>());
		when(paymentsService.getOrganizationReceipts(anyInt(),anyInt(),anyString(), anyString())).thenReturn(receipts);
		
		ResponseEntity<ReceiptsInfo> res = paymentsController.getOrganizationReceipts(anyString(),anyInt(),anyInt(), anyString());
		assertEquals(HttpStatus.OK, res.getStatusCode());
		
	}
	
	@Test
	void getOrganizationReceipts_404() throws Exception {
		//precondition
		doThrow(new AppException(AppError.RECEIPTS_NOT_FOUND, "111", 0)).when(paymentsService).getOrganizationReceipts(anyInt(),anyInt(),anyString(), anyString());
		try {
			paymentsController.getOrganizationReceipts(anyString(),anyInt(),anyInt(), anyString());
		} catch(AppException e) {
			assertEquals(HttpStatus.NOT_FOUND, e.getHttpStatus());
		}
		
	}

	
	
}