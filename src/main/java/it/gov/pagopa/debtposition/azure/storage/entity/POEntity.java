package it.gov.pagopa.debtposition.azure.storage.entity;

import java.time.LocalDateTime;

import com.microsoft.azure.storage.table.TableServiceEntity;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class POEntity extends TableServiceEntity {

    private String iupd;
    private String paymentDate;

    public POEntity(String organizationFiscalCode, String iuv, String iupd, String paymentDate) {

        this.partitionKey = organizationFiscalCode;
        this.rowKey = iuv;
        this.iupd = iupd;
        this.paymentDate = paymentDate;
    }

}
