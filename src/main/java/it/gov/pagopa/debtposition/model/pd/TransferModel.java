package it.gov.pagopa.debtposition.model.pd;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.validation.Valid;
import javax.validation.constraints.*;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class TransferModel implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 5593063492841435180L;

  @NotBlank(message = "id transfer is required")
  @Schema(
      type = "string",
      allowableValues = {"1", "2", "3", "4", "5"})
  private String idTransfer;

  @NotNull(message = "amount is required")
  @Min(value= 1L, message = "minimum amount is 1 eurocent")
  private Long amount;

  @Schema(
      description = "Fiscal code related to the organization targeted by this transfer.",
      example = "00000000000")
  private String organizationFiscalCode;

  @NotBlank(message = "remittance information is required")
  @Size(
      max = 140,
      message =
          "remittance information must be compliant to EACT FORMATTING RULES, up to 140 chars")
  // https://docs.pagopa.it/saci/specifiche-attuative-dei-codici-identificativi-di-versamento-riversamento-e-rendicontazione/operazione-di-trasferimento-fondi
  private String remittanceInformation; // causale

  @NotBlank(message = "category is required")
  private String category; // taxonomy

  @Schema(description = "mutual exclusive with stamp", example = "IT0000000000000000000000000")
  private String iban;

  @Schema(
      description = "optional - can be combined with iban but not with stamp",
      example = "IT0000000000000000000000000")
  private String postalIban;

  @Schema(description = "mutual exclusive with iban and postalIban")
  @Valid
  private Stamp stamp;

  @Size(max = 140)
  private String companyName;

  @Valid
  @Size(min = 0, max = 10)
  @Schema(description = "it can added a maximum of 10 key-value pairs for metadata")
  private List<TransferMetadataModel> transferMetadata = new ArrayList<>();

  public void addTransferMetadata(TransferMetadataModel trans) {
    transferMetadata.add(trans);
  }

  public void removeTransferMetadata(TransferMetadataModel trans) {
    transferMetadata.remove(trans);
  }
}
