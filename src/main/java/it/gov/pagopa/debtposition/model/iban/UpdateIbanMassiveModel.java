package it.gov.pagopa.debtposition.model.iban;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UpdateIbanMassiveModel {

    @JsonProperty("old_iban")
    private String oldIban;
    @JsonProperty("new_iban")
    private String newIban;

}
