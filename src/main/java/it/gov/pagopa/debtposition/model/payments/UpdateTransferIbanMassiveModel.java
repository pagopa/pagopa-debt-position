package it.gov.pagopa.debtposition.model.payments;

import javax.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UpdateTransferIbanMassiveModel {

  @NotNull private String oldIban;
  @NotNull private String newIban;
}
