package it.gov.pagopa.debtposition.model.payments;

import java.io.Serializable;
import java.time.LocalDateTime;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Schema(name = "PayPaymentOptionModel")
public class PaymentOptionModel implements Serializable {

	/**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = 661757182968854177L;

	@NotNull(message = "paymentDate is required")
	private LocalDateTime paymentDate;
	@NotBlank(message = "paymentMethod is required")
	private String paymentMethod;
	@NotBlank(message = "pspCompany is required")
	private String pspCompany;
	@NotBlank(message = "idReceipt is required")
	private String idReceipt;
}
