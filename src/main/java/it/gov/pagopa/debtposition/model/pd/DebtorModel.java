package it.gov.pagopa.debtposition.model.pd;

import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import java.io.Serializable;

@Data
@NoArgsConstructor
public class DebtorModel implements Serializable {

    @NotNull(message = "type is required")
    private Type type;
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
    @Pattern(regexp="[A-Z]{2}", message="The country must be reported with two capital letters (example: IT)")
    private String country;
    @Schema(example = "email@domain.com")
    @Email(message = "Please provide a valid email address")
    @ToString.Exclude
    private String email;
    @ToString.Exclude
    private String phone;
}
