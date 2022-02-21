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
    public static final String organizationKey = "organization";

    public OrganizationEntity(String organizationId, String organizationOnboardingDate) {
        this.partitionKey = organizationKey;
        this.rowKey = organizationId;
        this.organizationOnboardingDate = organizationOnboardingDate;
    }

    public OrganizationEntity(String organizationId) {
        this.partitionKey = organizationKey;
        this.rowKey = organizationId;
    }
}