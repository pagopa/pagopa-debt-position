package it.gov.pagopa.debtposition.model.payments.verify.response;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter 
@Setter 
@NoArgsConstructor 
@AllArgsConstructor 
@Builder
public class VerifyPaymentOptionsResponse {
	private String  paTaxCode;    
	private String  paFullName;  
	private String  paOfficeName;
	private Boolean standIn;
	private List<PaymentOptionGroup> paymentOptions;
}
