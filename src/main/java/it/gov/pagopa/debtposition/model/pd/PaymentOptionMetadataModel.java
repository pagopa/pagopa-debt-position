package it.gov.pagopa.debtposition.model.pd;

import java.io.Serializable;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentOptionMetadataModel implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 4575041445781686511L;

  @NotBlank(message = "key is required")
  @Size(max = 140, message = "key must not exceed 140 characters")
  private String key;

  @NotBlank(message = "value is required")
  @Size(max = 140, message = "value must not exceed 140 characters")
  private String value;
}
