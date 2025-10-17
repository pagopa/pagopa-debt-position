package it.gov.pagopa.debtposition.dto;

import java.util.List;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;
import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class MultipleIUPDDTO {

  @NotEmpty
  @Size(max = 100)
  private List<String> paymentPositionIUPDs;
}
