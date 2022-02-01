package it.gov.pagopa.debtposition.model.pd;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class TransferModel implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = 5593063492841435180L;
    
    @NotBlank(message = "id transfer is required")
    private String idTransfer;
    @NotBlank(message = "amount is required")
    private long amount;
    @NotBlank(message = "remittance information is required")
    private String remittanceInformation; // causale
    @NotBlank(message = "category is required")
    private String category; // taxonomy
    @NotBlank(message = "iban is required")
    private String iban;
    private String postalIban;
}
