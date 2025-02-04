package it.gov.pagopa.debtposition.model.pd;

import java.util.List;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class MultipleIUPDModel {

  @NotEmpty
  @Size(max = 20)
  @NotNull
  private List<String> paymentPositionIUPDs;
}
