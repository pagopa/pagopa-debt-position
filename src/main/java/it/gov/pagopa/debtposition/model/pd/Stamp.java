package it.gov.pagopa.debtposition.model.pd;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Stamp implements Serializable {
  private static final long serialVersionUID = -5862140737726963810L;

  @NotBlank
  @Size(max = 72)
  @Schema(
      required = true,
      description =
          "Document hash type is stBase64Binary72 as described in"
              + " https://github.com/pagopa/pagopa-api.")
  // Stamp generally get as input a base64sha256, that is the SHA256 hash of a given string encoded
  // with Base64.
  // It is not equivalent to base64encode(sha256(“test”)), if sha256() returns a hexadecimal
  // representation.
  // The result should normally be 44 characters, to be compliant with as-is it was extended to 72
  private String hashDocument;

  @NotBlank
  @Size(min = 2, max = 2)
  @Schema(required = true, description = "The type of the stamp", minLength = 2, maxLength = 2)
  private String stampType;

  @NotBlank
  @Pattern(regexp = "[A-Z]{2}")
  @Schema(
      required = true,
      description = "The provincial of the residence",
      example = "RM",
      pattern = "[A-Z]{2,2}")
  private String provincialResidence;
}
