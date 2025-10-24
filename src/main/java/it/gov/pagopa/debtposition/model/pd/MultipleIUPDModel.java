package it.gov.pagopa.debtposition.model.pd;

import java.util.List;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
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
