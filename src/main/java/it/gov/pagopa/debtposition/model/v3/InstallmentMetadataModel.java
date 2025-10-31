package it.gov.pagopa.debtposition.model.v3;

import java.io.Serializable;

import it.gov.pagopa.debtposition.model.Metadata;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InstallmentMetadataModel extends Metadata implements Serializable {

  @NotBlank(message = "key is required")
  private String key;

  private String value;
}
