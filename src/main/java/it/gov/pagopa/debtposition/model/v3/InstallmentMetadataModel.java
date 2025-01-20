package it.gov.pagopa.debtposition.model.v3;

import java.io.Serializable;
import javax.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InstallmentMetadataModel implements Serializable {

  @NotBlank(message = "key is required")
  private String key;

  private String value;
}
