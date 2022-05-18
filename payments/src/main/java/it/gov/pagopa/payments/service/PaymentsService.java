package it.gov.pagopa.payments.service;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Positive;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.ResultSegment;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.TableOperation;
import com.microsoft.azure.storage.table.TableQuery;
import com.microsoft.azure.storage.table.TableQuery.Operators;
import com.microsoft.azure.storage.table.TableQuery.QueryComparisons;

import feign.FeignException;
import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.entity.Status;
import it.gov.pagopa.payments.exception.AppError;
import it.gov.pagopa.payments.exception.AppException;
import it.gov.pagopa.payments.model.PaymentOptionStatus;
import it.gov.pagopa.payments.model.PaymentsModelResponse;
import it.gov.pagopa.payments.model.PaymentsResult;
import it.gov.pagopa.payments.utils.AzuriteStorageUtil;
import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class PaymentsService {

	
	private static final String PARTITION_KEY_FIELD = "PartitionKey";
	private static final String ROW_KEY_FIELD = "RowKey";
	private static final String DEBTOR_FIELD = "Debtor";
	private static final String STATUS_FIELD = "Status";
	private static final String[] columns = new String[]{PARTITION_KEY_FIELD, ROW_KEY_FIELD, DEBTOR_FIELD, STATUS_FIELD};
	
	@Value("${payments.sa.connection}")
	private String storageConnectionString;
	@Value("${receipts.table}")
	private String receiptsTable;
	
	@Autowired
    private GpdClient gpdClient;
	
	public PaymentsService() {}
	
	public PaymentsService(String storageConnectionString, String receiptsTable, GpdClient gpdClient) {
		this.storageConnectionString = storageConnectionString;
		this.receiptsTable = receiptsTable;
		this.gpdClient = gpdClient;
	}


	public ReceiptEntity getReceiptByOrganizationFCAndIUV(@Validated @NotBlank String organizationFiscalCode,
			@Validated @NotBlank String iuv) {
		
		final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s, iuv=%s";

		AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString);
		try {
			azuriteStorageUtil.createTable(receiptsTable);


			CloudTable table = CloudStorageAccount.parse(storageConnectionString)
					.createCloudTableClient()
					.getTableReference(receiptsTable);

			String partitionKeyFilter = TableQuery.generateFilterCondition(PARTITION_KEY_FIELD, TableQuery.QueryComparisons.EQUAL, organizationFiscalCode);
			String rowKeyFilter       = TableQuery.generateFilterCondition(ROW_KEY_FIELD, TableQuery.QueryComparisons.EQUAL, iuv);

			String filter = TableQuery.combineFilters(partitionKeyFilter, Operators.AND, rowKeyFilter);

			Iterable<ReceiptEntity> result = table.execute(TableQuery.from(ReceiptEntity.class).where(filter));
			
			if (!result.iterator().hasNext()) {
				throw new AppException(AppError.RECEIPT_NOT_FOUND, organizationFiscalCode, iuv);
			}
			
			ReceiptEntity receipt = result.iterator().next();
			
			// check debt position status on gpd 
			this.checkGPDDebtPosStatus(table, receipt);

			return receipt;
		}
		catch (InvalidKeyException | URISyntaxException | StorageException e) {
			log.error("[getReceiptByOrganizationFCAndIUV] Payments Generic Error " + String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iuv), e);
			throw new AppException(AppError.RETRIEVAL_RECEIPT_FAILED, organizationFiscalCode, iuv);
		}

	}
	
	public PaymentsResult<ReceiptEntity> getOrganizationReceipts(@Positive Integer limit, @Min(0) Integer pageNum, @NotBlank String organizationFiscalCode,
			String debtor, String service) {

		final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s, debtor=%s, service=%s";

		AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString);
		try {
			azuriteStorageUtil.createTable(receiptsTable);

			CloudTable table = CloudStorageAccount.parse(storageConnectionString)
					.createCloudTableClient()
					.getTableReference(receiptsTable);

			String filter = TableQuery.generateFilterCondition(PARTITION_KEY_FIELD, TableQuery.QueryComparisons.EQUAL, organizationFiscalCode);
			
			if (null != debtor) {
				String debtorFilter       = TableQuery.generateFilterCondition(DEBTOR_FIELD, TableQuery.QueryComparisons.EQUAL, debtor);
				filter = TableQuery.combineFilters(filter, Operators.AND, debtorFilter);
			}
			
			if (null != service) {
				String segretationCodeFilter  = PaymentsService.getStartsWithFilter(ROW_KEY_FIELD, service);
				filter = TableQuery.combineFilters(filter, Operators.AND, segretationCodeFilter );
			}

			PaymentsResult<ReceiptEntity> result;
			
			if (null == limit) {
				result = this.getNotSegmentedReceipts(table, filter);
			} else {
				result = this.getSegmentedReceipts(limit, pageNum, organizationFiscalCode, table, filter);
			}
			
			return this.getGPDCheckedReceiptsList(table, result);
		}
		catch (InvalidKeyException | URISyntaxException | StorageException e) {
			log.error("[getReceiptByOrganizationFCAndIUV] Payments Generic Error " + String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, debtor, service), e);
			throw new AppException(AppError.RETRIEVAL_RECEIPTS_FAILED, organizationFiscalCode, debtor);
		}

    }
	
	public void checkGPDDebtPosStatus (CloudTable table, ReceiptEntity receipt) {
		// the check on GPD is necessary if the status of the receipt is different from PAID
		if(!receipt.getStatus().trim().equalsIgnoreCase(Status.PAID.name())) {
			PaymentsModelResponse paymentOption = gpdClient.getPaymentOption(receipt.getPartitionKey(), receipt.getRowKey());
			if (null != paymentOption && !PaymentOptionStatus.PO_PAID.equals(paymentOption.getStatus())) {
				throw new AppException(AppError.UNPROCESSABLE_RECEIPT, paymentOption.getStatus(), receipt.getPartitionKey(), receipt.getRowKey());
			}
			// if no exception is raised the status on GPD is correctly in PAID -> for congruence update receipt status
			receipt.setStatus(Status.PAID.name());
			TableOperation updateOperation = TableOperation.merge(receipt);
	        try {
				table.execute(updateOperation);
			} catch (StorageException e) {
				log.error("[checkGPDDebtPosStatus] Non-blocking error: "
						+ "Exception during the update status in table "+ receiptsTable +" for ReceiptEntity [pk:"+receipt.getPartitionKey()+", rk:"+receipt.getRowKey()+"]", e);
			}   
		}	
	}
	
	public PaymentsResult<ReceiptEntity> getGPDCheckedReceiptsList(CloudTable table, PaymentsResult<ReceiptEntity> result){
		// for all the receipts in the azure table, only those that have been already PAID status or are in PAID status on GPD are returned
		List<ReceiptEntity> checkedReceipts = new ArrayList<>();
		for (ReceiptEntity re: result.getResults()) {
			try {
				this.checkGPDDebtPosStatus(table, re);
				checkedReceipts.add(re);
			} 
			catch (FeignException.NotFound e ) {
				log.error("[getGPDCheckedReceiptsList] Non-blocking error: "
						+ "get not found exception in the recovery of payment options", e);
			}
			catch (AppException e) {
				log.error("[getGPDCheckedReceiptsList] Non-blocking error: "
						+ "Receipt is not in an eligible state on GPD in order to be returned to the caller", e);
			}
		}
		result.setResults(checkedReceipts);
		result.setLength(checkedReceipts.size());
		return result;
	}
	
	
	private PaymentsResult<ReceiptEntity> getSegmentedReceipts(Integer limit, Integer pageNum, String organizationFiscalCode, CloudTable table, String filter) throws StorageException {
		final int FIRST_PAGE_NUMBER = 0;
		TableQuery<ReceiptEntity> tq = TableQuery.from(ReceiptEntity.class);
		tq.setColumns(columns);
		PaymentsResult<ReceiptEntity> result = new PaymentsResult<>();
		// first page results
		ResultSegment<ReceiptEntity> segmentedRes = table.executeSegmented(tq.select(columns).where(filter).take(limit), null);
		result.setLength(segmentedRes.getLength());
		result.setPageSize(segmentedRes.getPageSize());
		result.setCurrentPageNumber(FIRST_PAGE_NUMBER);
		result.setResults(segmentedRes.getResults());
		result.setHasMoreResults(segmentedRes.getHasMoreResults());
		
		if(result.getResults().isEmpty()) {
			throw new AppException(AppError.RECEIPTS_NOT_FOUND, organizationFiscalCode, pageNum);
		}
		
		if (pageNum > 0) {
			for (int i=1; i<=pageNum; i++) {
				if (segmentedRes.getHasMoreResults()) {
					segmentedRes = table.executeSegmented(TableQuery.from(ReceiptEntity.class).where(filter).take(limit), segmentedRes.getContinuationToken());
					result.setLength(segmentedRes.getLength());
					result.setPageSize(segmentedRes.getPageSize());
					result.setCurrentPageNumber(i);
					result.setResults(segmentedRes.getResults());
					result.setHasMoreResults(segmentedRes.getHasMoreResults());
					
				}
				else {
					throw new AppException(AppError.RECEIPTS_NOT_FOUND, organizationFiscalCode, pageNum);
				}
			}
		}
		
		return result;
	}

	private PaymentsResult<ReceiptEntity> getNotSegmentedReceipts(CloudTable table, String filter) {
		 final int FIRST_PAGE_NUMBER = 0;
		 TableQuery<ReceiptEntity> tq = TableQuery.from(ReceiptEntity.class);
		 tq.setColumns(columns);
		 PaymentsResult<ReceiptEntity> result = new PaymentsResult<>();
		 List<ReceiptEntity> listOfEntity = StreamSupport.stream(table.execute(tq.select(columns).where(filter)).spliterator(), false).collect(Collectors.toList());
		 result.setResults(listOfEntity);
		 result.setCurrentPageNumber(FIRST_PAGE_NUMBER);
		 result.setLength(listOfEntity.size());
		 result.setHasMoreResults(false);
		 return result;
	}
	
	private static String getStartsWithFilter(String columnName, String startsWith) {
        var length = startsWith.length() - 1;
        var nextChar = startsWith.toCharArray()[length] + 1;

        var startWithEnd = startsWith.substring(0, length) + (char) nextChar;
        return TableQuery.combineFilters(
            TableQuery.generateFilterCondition(columnName, QueryComparisons.GREATER_THAN_OR_EQUAL, startsWith),
            Operators.AND,
            TableQuery.generateFilterCondition(columnName, QueryComparisons.LESS_THAN, startWithEnd));
    }

}
