package it.gov.pagopa.debtposition.model.payments;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.time.LocalDateTime;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Schema(name = "AlreadyPaidPaymentOptionModel")
public class AlreadyPaidPaymentOptionModel implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 121803220168846009L;

  private LocalDateTime paymentDate;
}
