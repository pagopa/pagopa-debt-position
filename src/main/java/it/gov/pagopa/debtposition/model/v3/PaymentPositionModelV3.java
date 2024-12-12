package it.gov.pagopa.debtposition.model.v3;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.*;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class PaymentPositionModelV3 implements Serializable {

    @NotBlank(message = "iupd is required")
    private String iupd;
    @Schema(description = "feature flag to enable a debt position in stand-in mode", example = "true", defaultValue = "true")
    private boolean payStandIn = true;
    @NotBlank(message = "company name is required")
    @Size(max = 140) // compliant to paForNode.xsd
    private String companyName; // es. Comune di Roma
    @Size(max = 140) // compliant to paForNode.xsd
    private String officeName; // es. Ufficio Tributi
    @JsonProperty(access = Access.READ_ONLY)
    @Schema(accessMode = Schema.AccessMode.READ_ONLY)
    private LocalDateTime paymentDate;
    @JsonProperty(access = Access.READ_ONLY)
    @Schema(accessMode = Schema.AccessMode.READ_ONLY)
    private DebtPositionStatusV3 status;

    @Valid
    private List<@Valid PaymentOptionModelV3> paymentOption = new ArrayList<>();

    public void addPaymentOptions(PaymentOptionModelV3 paymentOpt) {
        paymentOption.add(paymentOpt);
    }

    public void removePaymentOptions(PaymentOptionModelV3 paymentOpt) {
        paymentOption.remove(paymentOpt);
    }
}
