package it.gov.pagopa.reporting.entity;

import com.microsoft.azure.storage.table.TableServiceEntity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class OrganizationEntity extends TableServiceEntity {

    private String organizationOnboardingDate;
    public static String myPartitionKey = "organization";

    public OrganizationEntity(String organizationId, String organizationOnboardingDate) {
        this.partitionKey = myPartitionKey;
        this.rowKey = organizationId;
        this.organizationOnboardingDate = organizationOnboardingDate;
    }

    public OrganizationEntity(String organizationId) {
        this.partitionKey = myPartitionKey;
        this.rowKey = organizationId;
    }
}