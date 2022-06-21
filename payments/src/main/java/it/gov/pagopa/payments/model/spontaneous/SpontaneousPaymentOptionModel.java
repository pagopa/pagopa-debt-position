package it.gov.pagopa.payments.model.spontaneous;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

@Data
@NoArgsConstructor
public class SpontaneousPaymentOptionModel implements Serializable {

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

    private List<TransferModel> transfer;
}
