package it.gov.pagopa.debtposition.model.pd.response;

import java.io.Serializable;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransferMetadataModelResponse implements Serializable {
  /** generated serialVersionUID */
  private static final long serialVersionUID = -8317651914223038540L;

  private String key;
  private String value;
}
