package it.gov.pagopa.debtposition.dto;

import java.io.Serializable;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PaymentOptionMetadataDTO implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -9190178547634083043L;

  private String key;
  private String value;
}
