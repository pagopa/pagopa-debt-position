package it.gov.pagopa.debtposition.model.pd;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class PaymentOptionModel implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -8328320637402363721L;

    @NotBlank(message = "iuv is required")
    private String iuv;
    @NotBlank(message = "amount is required")
    private long amount;
    private String description;
    @NotBlank(message = "is partial payment is required")
    private Boolean isPartialPayment;
    @NotBlank(message = "due date is required")
    private LocalDateTime dueDate;
    private LocalDateTime retentionDate;
    private long fee;

    private List<TransferModel> transfer = new ArrayList<>();

    public void addTransfers(TransferModel trans) {
        transfer.add(trans);
    }

    public void removeTransfers(TransferModel trans) {
        transfer.remove(trans);
    }
}
