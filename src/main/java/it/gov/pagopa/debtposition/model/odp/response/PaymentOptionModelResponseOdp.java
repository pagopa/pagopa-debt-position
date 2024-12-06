package it.gov.pagopa.debtposition.model.odp.response;

import it.gov.pagopa.debtposition.model.odp.InstallmentModel;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class PaymentOptionModelResponseOdp implements Serializable {

    private LocalDateTime dueDate;
    private LocalDateTime retentionDate;
    private LocalDateTime insertedDate;
    
    private List<InstallmentModelResponse> installments = new ArrayList<>();
}
