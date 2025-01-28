package it.gov.pagopa.debtposition.model.payments;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UpdateTransferIbanMassiveModel {

    @NotNull
    @JsonProperty("old_iban")
    private String oldIban;
    @NotNull
    @JsonProperty("new_iban")
    private String newIban;

}
