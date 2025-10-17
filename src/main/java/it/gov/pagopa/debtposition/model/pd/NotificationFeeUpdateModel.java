package it.gov.pagopa.debtposition.model.pd;

import java.io.Serializable;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class NotificationFeeUpdateModel implements Serializable {

  /** */
  private static final long serialVersionUID = -6961547642494729235L;

  @NotNull(message = "Notification fee is required")
  @PositiveOrZero(message = "Notification fee must be greater or equals to zero")
  private Long notificationFee;
}
