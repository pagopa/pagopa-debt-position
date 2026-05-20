package it.gov.pagopa.debtposition.model.v3;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.controller.pd.validator.UniqueMetadataKeys;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class InstallmentModel implements Serializable {

  private static final long serialVersionUID = 3511124075334907970L;

  private String nav;

  @NotBlank(message = "iuv is required")
  private String iuv;

  @NotNull(message = "amount is required")
  @Min(value= 1L, message = "minimum amount is 1 eurocent")
  private Long amount;

  @NotBlank(message = "payment option description is required")
  @Size(max = 140) // compliant to paForNode.xsd
  private String description;

  @NotNull(message = "due date is required")
  private LocalDateTime dueDate;

  @JsonProperty(access = JsonProperty.Access.READ_ONLY)
  @Schema(accessMode = Schema.AccessMode.READ_ONLY)
  private long fee;

  @JsonProperty(access = JsonProperty.Access.READ_ONLY)
  @Schema(accessMode = Schema.AccessMode.READ_ONLY)
  private long notificationFee;

  @JsonProperty(access = JsonProperty.Access.READ_ONLY)
  @Schema(accessMode = Schema.AccessMode.READ_ONLY)
  private InstallmentStatus status;

  @Valid
  @Size(min = 1)
  private List<TransferModel> transfer = new ArrayList<>();

  @Valid
  @Size(min = 0, max = 10)
  // Metadata keys must be unique within a single installment to match the database constraint.
  @UniqueMetadataKeys(message = "installmentMetadata keys must be unique")
  @Schema(
      description =
          "It can be added a maximum of 10 key-value pairs for metadata. Metadata keys must be unique within the same installment.")
  @ArraySchema(uniqueItems = true)
  private List<InstallmentMetadataModel> installmentMetadata = new ArrayList<>();
}
