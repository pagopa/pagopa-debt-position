package it.gov.pagopa.debtposition.model.checkposition;

import java.io.Serializable;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NodeCheckPositionModel implements Serializable {
  /** generated serialVersionUID */
  private static final long serialVersionUID = -8146130541120434068L;

  private List<NodePosition> positionslist;
}
