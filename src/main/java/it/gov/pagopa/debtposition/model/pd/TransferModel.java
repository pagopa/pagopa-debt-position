package it.gov.pagopa.debtposition.model.pd;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

@Data
@NoArgsConstructor
public class TransferModel implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = 5593063492841435180L;

    @NotBlank(message = "id transfer is required")
    @Schema(type = "string", allowableValues = {"1", "2", "3", "4", "5"})
    private String idTransfer;

    @NotNull(message = "amount is required")
    private Long amount;

    @Schema(description = "Fiscal code related to the organization targeted by this transfer.", example = "00000000000")
    private String organizationFiscalCode;

    @NotBlank(message = "remittance information is required")
    private String remittanceInformation; // causale

    @NotBlank(message = "category is required")
    private String category; // taxonomy

    @Schema(description = "mutual exclusive with postalIban and stamp", example = "IT0000000000000000000000000")
    private String iban;

    @Schema(description = "mutual exclusive with iban and stamp", example = "IT0000000000000000000000000")
    private String postalIban;

    @Schema(description = "mutual exclusive with iban and postalIban")
    private Stamp stamp;

}
