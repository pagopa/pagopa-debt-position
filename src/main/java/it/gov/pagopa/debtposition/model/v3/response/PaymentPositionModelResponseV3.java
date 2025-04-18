package it.gov.pagopa.debtposition.model.v3.response;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentPositionModelResponseV3 implements Serializable {

  private String iupd;
  private String organizationFiscalCode;
  private String companyName; // es. Comune di Roma
  private String officeName; // es. Ufficio Tributi
  private LocalDateTime insertedDate;
  private LocalDateTime publishDate;
  private LocalDateTime paymentDate;
  private DebtPositionStatusV3 status;
  private LocalDateTime lastUpdatedDate;

  private List<PaymentOptionModelResponseV3> paymentOption = new ArrayList<>();
}
