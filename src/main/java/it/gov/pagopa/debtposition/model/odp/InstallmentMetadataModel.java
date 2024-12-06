package it.gov.pagopa.debtposition.model.odp;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

public class InstallmentMetadataModel implements Serializable {

    @NotBlank(message = "key is required")
    private String key;
    private String value;
}
