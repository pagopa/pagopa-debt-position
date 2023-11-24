package it.gov.pagopa.debtposition.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;

import it.gov.pagopa.debtposition.model.pd.MultiplePaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class MultiplePaymentPositionDTO implements Serializable{
	
	/**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = 3144050302407930549L;
		
	@Valid
	@NotEmpty
	private List<PaymentPositionDTO> paymentPositions;

}
