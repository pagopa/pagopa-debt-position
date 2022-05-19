package it.gov.pagopa.payments.entity;

import com.microsoft.azure.storage.table.TableServiceEntity;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class ReceiptEntity extends TableServiceEntity {

    private String debtor;
    private String document;
    private String status = Status.CREATED.name();
   


    public ReceiptEntity(String organizationFiscalCode, String iuv) {
        this.partitionKey = organizationFiscalCode;
        this.rowKey = iuv;
        // https://docs.microsoft.com/en-us/dotnet/api/microsoft.azure.cosmos.table.tableentity.etag?view=azure-dotnet#microsoft-azure-cosmos-table-tableentity-etag
        this.etag = "*";
    }

}
