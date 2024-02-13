package it.gov.pagopa.debtposition.azure.storage.entity;

import com.microsoft.azure.storage.table.TableServiceEntity;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PPEntity extends TableServiceEntity {

    private String paymentPosition;

    public PPEntity(String organizationFiscalCode, String iupd, String paymentPosition) {
        this.partitionKey = organizationFiscalCode;
        this.rowKey = iupd;
        this.paymentPosition = paymentPosition;
    }

}
