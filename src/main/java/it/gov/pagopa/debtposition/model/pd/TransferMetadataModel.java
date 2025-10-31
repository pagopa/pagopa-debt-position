package it.gov.pagopa.debtposition.model.pd;

import java.io.Serializable;

import it.gov.pagopa.debtposition.model.Metadata;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Model representing a metadata entry for a transfer.
 * <p>
 * This class is aligned with the PagoPA SANP specification
 * at:
 * https://github.com/pagopa/pagopa-api/blob/b70d47bfae96a4c8782ed3f78305126a0601ff46/xsd-common/sac-common-types-1.0.xsd#L104
 * <p>
 * According to the SANP:
 * - Both {@code key} and {@code value} are required.
 * - Maximum length: 140 characters.
 */


@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransferMetadataModel extends Metadata implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -1509450417943158597L;

  @NotBlank(message = "key is required")
  @Size(max = 140, message = "key must not exceed 140 characters")
  private String key;

  @NotBlank(message = "value is required")
  @Size(max = 140, message = "value must not exceed 140 characters")
  private String value;
}
