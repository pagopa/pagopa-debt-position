package it.gov.pagopa.debtposition.model.pd;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.util.LocalDateTimeDeserializer;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentOptionModel implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -8328320637402363721L;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private String nav;
    @NotBlank(message = "iuv is required")
    private String iuv;
    @NotNull(message = "amount is required")
    private Long amount;
    private String description;
    @NotNull(message = "is partial payment is required")
    private Boolean isPartialPayment;
    @Schema(description = "date by which the debt position must be paid", pattern = "[yyyy-MM-ddTHH:mm:ss.SSSZ] or [yyyy-MM-ddTHH:mm:ss]")
    @NotNull(message = "due date is required")
    @JsonDeserialize(using = LocalDateTimeDeserializer .class)
    private LocalDateTime dueDate;
    @Schema(description = "date by which the debt position can still be paid after its expiry", pattern = "[yyyy-MM-ddTHH:mm:ss.SSSZ] or [yyyy-MM-ddTHH:mm:ss]")
    @JsonDeserialize(using = LocalDateTimeDeserializer .class)
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
