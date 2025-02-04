package it.gov.pagopa.debtposition.model.v3;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentOptionModelV3 implements Serializable {

  @Size(max = 140) // todo save also this value, the installment property is used
  @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
  private String description;

  private LocalDateTime validityDate;
  private LocalDateTime retentionDate;

  @Schema(
      description = "feature flag to enable the payment option to expire after the due date",
      example = "false",
      defaultValue = "false")
  @NotNull(message = "switch to expired value is required")
  private Boolean switchToExpired;

  @Valid @NotNull private DebtorModel debtor;

  @Valid
  @NotNull
  @Size(min = 1, max = 100)
  private List<InstallmentModel> installments = new ArrayList<>();

  public void addInstallment(InstallmentModel inst) {
    installments.add(inst);
  }
}
