package it.gov.pagopa.payments.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.time.LocalDateTime;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PaymentOptionModel implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7904798992919216489L;

	@NotNull(message = "paymentDate is required")
    private LocalDateTime paymentDate;

	@NotBlank(message = "paymentMethod is required")
    private String paymentMethod;

	@NotBlank(message = "pspCompany is required")
    private String pspCompany;

	@NotBlank(message = "idReceipt is required")
    private String idReceipt;
	
	@NotBlank(message = "fee is required")
	@Builder.Default
    private String fee = "0";
}
