package it.gov.pagopa.reporting.entity;

import com.microsoft.azure.storage.table.TableServiceEntity;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FlowEntity extends TableServiceEntity {

    private String flowDate;

    public FlowEntity(String flowId, String flowDate, String idPA) {

        this.partitionKey = idPA;
        this.rowKey = flowId;
        this.flowDate = flowDate;
    }

}