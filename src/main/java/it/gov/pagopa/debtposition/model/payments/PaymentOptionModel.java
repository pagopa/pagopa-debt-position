package it.gov.pagopa.debtposition.model.payments;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.time.LocalDateTime;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Schema(name = "PayPaymentOptionModel")
public class PaymentOptionModel implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 661757182968854177L;

  private LocalDateTime paymentDate;
  private String paymentMethod;

  // idPSP https://github.com/pagopa/pagopa-api/blob/c752179c66da9e3a2a71dd16397fde6b0ad08818/wsdl/xsd/paForNode.xsd#L219 stText35
  @Size(max = 35)
  private String pspCode;

  // pspFiscalCode https://github.com/pagopa/pagopa-api/blob/c752179c66da9e3a2a71dd16397fde6b0ad08818/wsdl/xsd/paForNode.xsd#L220 stText70
  @Size(max = 70)
  private String pspTaxCode;

  @NotBlank(message = "pspCompany is required")
  private String pspCompany;

  @NotBlank(message = "idReceipt is required")
  private String idReceipt;

  private String fee = "0";
}
