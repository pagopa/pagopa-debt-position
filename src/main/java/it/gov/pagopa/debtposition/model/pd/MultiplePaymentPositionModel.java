package it.gov.pagopa.debtposition.model.pd;

import java.io.Serializable;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;

import it.gov.pagopa.debtposition.controller.pd.validator.ValidDebtPositionsSize;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class MultiplePaymentPositionModel implements Serializable{
	
	/**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = -928726282710084991L;
	
	@Valid
	@NotEmpty
	@ValidDebtPositionsSize(max = 20)
	private List<PaymentPositionModel> paymentPositions;
}
