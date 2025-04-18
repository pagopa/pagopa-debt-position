package it.gov.pagopa.debtposition.model.send.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GetNotificationFeeResponse implements Serializable {
    private long notificationFeeAmount; // todo update with actual response
}
