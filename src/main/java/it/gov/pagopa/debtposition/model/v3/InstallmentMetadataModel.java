package it.gov.pagopa.debtposition.model.v3;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

@Data
@NoArgsConstructor
public class InstallmentMetadataModel implements Serializable {

    @NotBlank(message = "key is required")
    private String key;
    private String value;
}
