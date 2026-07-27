package it.gov.pagopa.debtposition.model.payments;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Schema(name = "ReportTransferRequest")
public class ReportTransferRequest implements Serializable {

  private static final long serialVersionUID = 125317043613535466L;

  @Size(max = 35)
  @Schema(
      description =
          "IUR (Identificativo Univoco Riscossione) of the reporting flow. When provided, "
              + "GPD resolves the payment option by IUR (matching the payment option idReceipt) "
              + "instead of the (organizationFiscalCode, IUV) pair.")
  private String iur;
}