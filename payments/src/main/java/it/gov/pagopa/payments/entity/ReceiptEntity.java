package it.gov.pagopa.payments.entity;

import com.microsoft.azure.storage.table.TableServiceEntity;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@Builder
public class ReceiptEntity extends TableServiceEntity {

    private String corporate;
    private String document;


    public ReceiptEntity(String organizationFiscalCode, String iuv) {
        this.partitionKey = organizationFiscalCode;
        this.rowKey = iuv;
        // https://docs.microsoft.com/en-us/dotnet/api/microsoft.azure.cosmos.table.tableentity.etag?view=azure-dotnet#microsoft-azure-cosmos-table-tableentity-etag
        this.etag = "*";
    }

}
