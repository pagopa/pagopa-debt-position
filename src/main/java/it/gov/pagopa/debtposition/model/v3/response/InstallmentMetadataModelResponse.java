package it.gov.pagopa.debtposition.model.v3.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InstallmentMetadataModelResponse implements Serializable {
    private String key;
    private String value;
}
