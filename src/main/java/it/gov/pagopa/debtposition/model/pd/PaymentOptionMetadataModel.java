package it.gov.pagopa.debtposition.model.pd;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentOptionMetadataModel implements Serializable {

    /**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = 4575041445781686511L;

	@NotBlank(message = "key is required")
    private String key;

    private String value;
}
