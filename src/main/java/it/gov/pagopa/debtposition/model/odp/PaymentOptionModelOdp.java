package it.gov.pagopa.debtposition.model.odp;

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
    @NotNull(message = "due date is required")
    private LocalDateTime dueDate;
    private LocalDateTime validityDate;
    private LocalDateTime retentionDate;

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
