package it.gov.pagopa.reporting.entities;

import com.microsoft.azure.storage.table.TableServiceEntity;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FlowEntity extends TableServiceEntity {

    private String flowDate;

    public FlowEntity(String flowId, String flowDate, String idPA) {
        this.partitionKey = idPA;
        this.rowKey = flowId;
        this.flowDate = flowDate;
        this.etag = "*";
    }

}