package it.gov.pagopa.debtposition.model.pd.response;

import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class PaymentPositionModelEnhancedResponse extends PaymentPositionModelBaseResponse {

  private Boolean switchToExpired;
  private DebtorModel debtor;
}
