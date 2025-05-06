package it.gov.pagopa.debtposition.model.pd;

import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import java.io.Serializable;
import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
public class DebtorModel implements Serializable {

  private static final long serialVersionUID = -3988004044071493474L;

@NotNull(message = "type is required")
  private Type type;

  @NotNull(message = "fiscal code is required")
  @ToString.Exclude
  private String fiscalCode;

  @NotNull(message = "full name is required")
  @ToString.Exclude
  private String fullName;

  @ToString.Exclude private String streetName;
  @ToString.Exclude private String civicNumber;
  @ToString.Exclude private String postalCode;
  @ToString.Exclude private String city;
  @ToString.Exclude private String province;
  @ToString.Exclude private String region;

  @ToString.Exclude
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
}
