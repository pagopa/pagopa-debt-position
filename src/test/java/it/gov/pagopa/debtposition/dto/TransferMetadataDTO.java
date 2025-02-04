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
public class TransferMetadataDTO implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 4523009354632595432L;

  private String key;
  private String value;
}
