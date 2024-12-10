package it.gov.pagopa.debtposition.model.odp;

import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class PaymentOptionModelOdp implements Serializable {

    @Size(max = 140) // todo use this field: is used that at the installment level
    private String description;
    private LocalDateTime validityDate;
    private LocalDateTime retentionDate;
    @Schema(description = "feature flag to enable the payment option to expire after the due date", example = "false", defaultValue = "false")
    @NotNull(message = "switch to expired value is required")
    private Boolean switchToExpired;

    @Valid
    private DebtorModel debtor;

    @Valid
    private List<InstallmentModel> installments = new ArrayList<>();

    public void addInstallment(InstallmentModel inst) {
        installments.add(inst);
    }

    public void removeTransfers(InstallmentModel inst) {
        installments.remove(inst);
    }
}
