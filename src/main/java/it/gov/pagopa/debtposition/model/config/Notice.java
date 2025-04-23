package it.gov.pagopa.debtposition.model.config;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;

@Data
@NoArgsConstructor
public class Notice {

    @NotNull(message = "Notice Number is required")
    private String nav;

    @NotNull(message = "Organization fiscal code is required")
    private String organizationFiscalCode;
}
