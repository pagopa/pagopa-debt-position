package it.gov.pagopa.debtposition.model.checkposition;

import java.io.Serializable;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NodePosition implements Serializable {
  /** generated serialVersionUID */
  private static final long serialVersionUID = 6608225665140949897L;

  private String fiscalCode;
  private String noticeNumber;
}
