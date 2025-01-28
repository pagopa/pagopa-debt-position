package it.gov.pagopa.debtposition.model.payments;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UpdateTransferIbanMassiveModel {

    @NotNull
    private String oldIban;
    @NotNull
    private String newIban;

}
