package it.gov.pagopa.debtposition.model.odp;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class InstallmentModel implements Serializable {

    private String nav;
    @NotBlank(message = "iuv is required")
    private String iuv;
    @NotNull(message = "amount is required")
    private Long amount;
    @NotBlank(message = "payment option description is required")
    @Size(max = 140) // compliant to paForNode.xsd
    private String description;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private long fee;
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    @Schema(accessMode = Schema.AccessMode.READ_ONLY)
    private long notificationFee;

    @Valid
    private List<TransferModel> transfer = new ArrayList<>();

    @Valid
    @Size(min=0, max=10)
    @Schema(description = "it can added a maximum of 10 key-value pairs for metadata")
    private List<InstallmentMetadataModel> installmentMetadata = new ArrayList<>();

    public void addTransfers(TransferModel trans) {
        transfer.add(trans);
    }

    public void removeTransfers(TransferModel trans) {
        transfer.remove(trans);
    }

    public void addPaymentOptionMetadata(InstallmentMetadataModel metadataModel) {
        installmentMetadata.add(metadataModel);
    }

    public void removePaymentOptionMetadata(InstallmentMetadataModel metadataModel) {
        installmentMetadata.remove(metadataModel);
    }
}