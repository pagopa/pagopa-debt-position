package it.gov.pagopa.debtposition.model.pd;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.PositiveOrZero;
import java.io.Serializable;

@Data
@NoArgsConstructor
public class NotificationFeeUpdateModel  implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = -6961547642494729235L;
	
	@NotNull(message = "Notification fee is required")
    @PositiveOrZero(message = "Notification fee must be greater or equals to zero")
    private Long notificationFee;
}
