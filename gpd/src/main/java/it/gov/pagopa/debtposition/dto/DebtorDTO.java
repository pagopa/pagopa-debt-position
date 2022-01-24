package it.gov.pagopa.debtposition.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.debtposition.model.enumeration.Type;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class DebtorDTO implements Serializable {

	/**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = 8220991882365625247L;
	
    private Type type;
	private String fiscalCode;
	private String fullName;
	private String streetName;
	private String civicNumber;
	private String postalCode;
	private String city;
	private String province;
	private String region;
	private String country;
	private String email;
	private String phone;
  
    private List<PaymentPositionDTO> paymentPosition = new ArrayList<>();
    
    public void addPaymentPosition(PaymentPositionDTO paymentPos) {
		paymentPosition.add(paymentPos);
	}

	public void removePaymentPosition(PaymentPositionDTO paymentPos) {
		paymentPosition.remove(paymentPos);
	}
}
