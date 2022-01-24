package it.gov.pagopa.debtposition.dto;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentPositionDTO implements Serializable {

    /**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = 6990851241801808964L;
	
	private String iupd;
    private String organizationFiscalCode;
    private String companyName; // es. Comune di Roma
    private String officeName; // es. Ufficio Tributi
    private LocalDateTime insertedDate;
    private LocalDateTime publishDate;
    private LocalDateTime validityDate;
    private DebtPositionStatus status;

    private List<PaymentOptionDTO> paymentOption = new ArrayList<>();
    
    public void addPaymentOptions(PaymentOptionDTO paymentOpt) {
		paymentOption.add(paymentOpt);
	}

	public void removePaymentOptions(PaymentOptionDTO paymentOpt) {
		paymentOption.remove(paymentOpt);
	}
}
