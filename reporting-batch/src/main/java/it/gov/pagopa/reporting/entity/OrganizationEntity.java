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
    public static final String ORGANIZATION_KEY = "organization";

    public OrganizationEntity(String organizationId, String organizationOnboardingDate) {
        this.partitionKey = ORGANIZATION_KEY;
        this.rowKey = organizationId;
        this.organizationOnboardingDate = organizationOnboardingDate;
    }

    public OrganizationEntity(String organizationId) {
        this.partitionKey = ORGANIZATION_KEY;
        this.rowKey = organizationId;
        // https://docs.microsoft.com/en-us/dotnet/api/microsoft.azure.cosmos.table.tableentity.etag?view=azure-dotnet#microsoft-azure-cosmos-table-tableentity-etag
        this.etag = "*";
    }
}