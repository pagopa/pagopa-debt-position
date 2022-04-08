package it.gov.pagopa.payments.service;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Positive;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.ResultSegment;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.TableQuery;
import com.microsoft.azure.storage.table.TableQuery.Operators;

import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.exception.AppError;
import it.gov.pagopa.payments.exception.AppException;
import it.gov.pagopa.payments.model.PaymentsResultSegment;
import it.gov.pagopa.payments.utils.AzuriteStorageUtil;
import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class PaymentsService {

	@Value("${payments.sa.connection}")
	private String storageConnectionString;
	@Value("${receipts.table}")
	private String receiptsTable;


	public ReceiptEntity getReceiptByOrganizationFCAndIUV(@NotBlank String organizationFiscalCode,
			@NotBlank String iuv) {
		
		final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s, iuv=%s";

		AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString);
		try {
			azuriteStorageUtil.createTable(receiptsTable);


			CloudTable table = CloudStorageAccount.parse(storageConnectionString)
					.createCloudTableClient()
					.getTableReference(receiptsTable);

			String partitionKeyFilter = TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, organizationFiscalCode);
			String rowKeyFilter       = TableQuery.generateFilterCondition("RowKey", TableQuery.QueryComparisons.EQUAL, iuv);

			String filter = TableQuery.combineFilters(partitionKeyFilter, Operators.AND, rowKeyFilter);

			Iterable<ReceiptEntity> result = table.execute(TableQuery.from(ReceiptEntity.class).where(filter));
			
			if (!result.iterator().hasNext()) {
				throw new AppException(AppError.RECEIPT_NOT_FOUND, organizationFiscalCode, iuv);
			}

			return result.iterator().next();
		}
		catch (InvalidKeyException | URISyntaxException | StorageException e) {
			log.error("[getReceiptByOrganizationFCAndIUV] Payments Generic Error " + String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iuv), e);
			throw new AppException(AppError.RETRIEVAL_RECEIPT_FAILED, organizationFiscalCode, iuv);
		}

	}
	
	public ResultSegment<ReceiptEntity> getOrganizationReceipts(@Positive Integer limit, @Positive Integer pageNum, @NotBlank String organizationFiscalCode,
			String debtor) {

		final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s, debtor=%s";

		AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString);
		try {
			azuriteStorageUtil.createTable(receiptsTable);


			CloudTable table = CloudStorageAccount.parse(storageConnectionString)
					.createCloudTableClient()
					.getTableReference(receiptsTable);

			String filter = TableQuery.generateFilterCondition("PartitionKey", TableQuery.QueryComparisons.EQUAL, organizationFiscalCode);
			if (null != debtor) {
				String debtorFilter       = TableQuery.generateFilterCondition("Debtor", TableQuery.QueryComparisons.EQUAL, debtor);
				filter = TableQuery.combineFilters(filter, Operators.AND, debtorFilter);
			}

			String[] columns = new String[]{"PartitionKey", "RowKey", "Debtor"};
			TableQuery<ReceiptEntity> tq = TableQuery.from(ReceiptEntity.class);
			tq.setColumns(columns);

			// first page results
			ResultSegment<ReceiptEntity> result = table.executeSegmented(tq.select(columns).where(filter).take(limit), null);
			
			if (pageNum > 0) {
				for (int i=1; i<=pageNum && result.getHasMoreResults(); i++) {
					result = table.executeSegmented(TableQuery.from(ReceiptEntity.class).where(filter).take(limit), result.getContinuationToken());
				}
			}
			
			return result;
		}
		catch (InvalidKeyException | URISyntaxException | StorageException e) {
			log.error("[getReceiptByOrganizationFCAndIUV] Payments Generic Error " + String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, debtor), e);
			throw new AppException(AppError.RETRIEVAL_RECEIPTS_FAILED, organizationFiscalCode, debtor);
		}

    }

}
