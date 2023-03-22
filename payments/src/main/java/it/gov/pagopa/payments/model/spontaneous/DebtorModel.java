package it.gov.pagopa.payments.model.spontaneous;

import it.gov.pagopa.payments.model.Type;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@Builder
public class DebtorModel implements Serializable {


    @NotNull(message = "type is required")
    private Type type;

    @NotBlank(message = "fiscal code is required")
    private String fiscalCode;

    @NotBlank(message = "full name is required")
    private String fullName;

    private String streetName;

    private String civicNumber;

    private String postalCode;

    private String city;

    private String province;

    private String region;

    @Pattern(regexp="[A-Z]{2}", message="The country must be reported with two capital letters (example: IT)")
    private String country;

    @Email(message = "Please provide a valid email address")
    private String email;

    private String phone;

}
