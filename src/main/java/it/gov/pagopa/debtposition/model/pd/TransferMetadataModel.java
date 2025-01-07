package it.gov.pagopa.debtposition.model.pd;

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
public class TransferMetadataModel implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -1509450417943158597L;

  @NotBlank(message = "key is required")
  private String key;

  private String value;
}
