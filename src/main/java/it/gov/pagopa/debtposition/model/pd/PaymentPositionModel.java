package it.gov.pagopa.debtposition.model.pd;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.IPaymentPositionModel;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
public class PaymentPositionModel implements Serializable, IPaymentPositionModel {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 1509046053787358148L;

  @NotBlank(message = "iupd is required")
  private String iupd;

  @NotNull(message = "type is required")
  private Type type;

  @Schema(
      description = "feature flag to enable a debt position in stand-in mode",
      example = "true",
      defaultValue = "true")
  private boolean payStandIn = true;

  @NotNull(message = "fiscal code is required")
  private String fiscalCode;

  @NotNull(message = "full name is required")
  @ToString.Exclude
  private String fullName;

  private String streetName;
  private String civicNumber;
  private String postalCode;
  private String city;
  private String province;
  private String region;

  @Schema(example = "IT")
  @Pattern(
      regexp = "[A-Z]{2}",
      message = "The country must be reported with two capital letters (example: IT)")
  private String country;

  @Schema(example = "email@domain.com")
  @Email(message = "Please provide a valid email address")
  @ToString.Exclude
  private String email;

  @ToString.Exclude private String phone;

  @Schema(
      description = "feature flag to enable the debt position to expire after the due date",
      example = "false",
      defaultValue = "false")
  @NotNull(message = "switch to expired value is required")
  private Boolean switchToExpired;

  // Payment Position properties
  @NotBlank(message = "company name is required")
  @Size(max = 140) // compliant to paForNode.xsd
  private String companyName; // es. Comune di Roma

  @Size(max = 140) // compliant to paForNode.xsd
  private String officeName; // es. Ufficio Tributi

  private LocalDateTime validityDate;

  @JsonProperty(access = Access.READ_ONLY)
  private LocalDateTime paymentDate;

  @JsonProperty(access = Access.READ_ONLY)
  private DebtPositionStatus status;

  @Valid private List<@Valid PaymentOptionModel> paymentOption = new ArrayList<>();

  public void addPaymentOptions(PaymentOptionModel paymentOpt) {
    paymentOption.add(paymentOpt);
  }

  public void removePaymentOptions(PaymentOptionModel paymentOpt) {
    paymentOption.remove(paymentOpt);
  }
}
