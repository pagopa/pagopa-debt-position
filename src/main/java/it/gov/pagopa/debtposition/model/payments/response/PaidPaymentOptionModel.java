package it.gov.pagopa.debtposition.model.payments.response;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Schema(name = "PaymentsModelResponse")
public class PaidPaymentOptionModel extends PaymentOptionModelResponse {

  private String serviceType;
}
