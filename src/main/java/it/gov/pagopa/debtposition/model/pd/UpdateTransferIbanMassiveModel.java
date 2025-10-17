package it.gov.pagopa.debtposition.model.pd;

import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UpdateTransferIbanMassiveModel {

  @NotNull private String newIban;
}
