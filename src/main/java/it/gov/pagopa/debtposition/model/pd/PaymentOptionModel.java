package it.gov.pagopa.debtposition.model.pd;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.Hidden;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
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
    @NotNull(message = "amount is required")
    private Long amount;
    private String description;
    @NotNull(message = "is partial payment is required")
    private Boolean isPartialPayment;
    @NotNull(message = "due date is required")
    private LocalDateTime dueDate;
    private LocalDateTime retentionDate;
    private long fee;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    @Schema(accessMode = Schema.AccessMode.READ_ONLY)
    private long notificationFee;

    @Valid
    private List<TransferModel> transfer = new ArrayList<>();

    public void addTransfers(TransferModel trans) {
        transfer.add(trans);
    }

    public void removeTransfers(TransferModel trans) {
        transfer.remove(trans);
    }
}
