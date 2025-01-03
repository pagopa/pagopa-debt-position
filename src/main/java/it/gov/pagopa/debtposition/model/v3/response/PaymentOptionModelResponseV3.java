package it.gov.pagopa.debtposition.model.v3.response;

import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class PaymentOptionModelResponseV3 implements Serializable {
    private Boolean switchToExpired;
    private LocalDateTime retentionDate;
    private LocalDateTime insertedDate;
    private LocalDateTime validityDate;
    private DebtorModel debtor;
    private List<InstallmentModelResponse> installments = new ArrayList<>();
}
