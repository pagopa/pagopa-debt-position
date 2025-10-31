package it.gov.pagopa.debtposition.model.payments.verify.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.v3.oas.annotations.media.Schema;
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
	private String  organizationFiscalCode;    
	private String  companyName;  
	private String  officeName;
	@Schema(hidden = true)
	@JsonIgnore
	@JsonProperty("standin")
	private Boolean standIn;
	private List<PaymentOptionGroup> paymentOptions;
}
