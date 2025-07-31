package it.gov.pagopa.debtposition.model.v3;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.controller.pd.validator.v3.ValidInstallmentsSize;
import it.gov.pagopa.debtposition.model.IPaymentPositionModel;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import javax.validation.Valid;
import javax.validation.constraints.*;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@ValidInstallmentsSize
public class PaymentPositionModelV3 implements Serializable, IPaymentPositionModel {

  @NotBlank(message = "IUPD (debt-position identifier) is required")
  private String iupd;

  @Schema(
      description = "feature flag to enable a debt position in stand-in mode",
      example = "true",
      defaultValue = "true")
  private boolean payStandIn = true;

  @NotBlank(message = "company name is required")
  @Size(max = 140) // compliant to paForNode.xsd
  private String companyName; // e.g. Comune di Roma

  @Size(max = 140) // compliant to paForNode.xsd
  private String officeName; // e.g. Ufficio Tributi

  @JsonProperty(access = Access.READ_ONLY)
  @Schema(accessMode = Schema.AccessMode.READ_ONLY)
  private LocalDateTime paymentDate;

  @JsonProperty(access = Access.READ_ONLY)
  @Schema(accessMode = Schema.AccessMode.READ_ONLY)
  private DebtPositionStatusV3 status;

  @Valid
  @NotNull
  @Size(min = 1, max = 100)
  private List<@Valid PaymentOptionModelV3> paymentOption = new ArrayList<>();

  public void addPaymentOption(PaymentOptionModelV3 paymentOpt) {
    paymentOption.add(paymentOpt);
  }
}
