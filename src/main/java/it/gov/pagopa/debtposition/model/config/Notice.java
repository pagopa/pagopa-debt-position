package it.gov.pagopa.debtposition.model.config;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotNull;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Notice {

    @NotNull(message = "Organization fiscal code is required")
    private String organizationFiscalCode;

    @NotNull(message = "Notice Number is required")
    private String nav;
}
