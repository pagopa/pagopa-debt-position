package it.gov.pagopa.debtposition.model.send.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.ZonedDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NotificationPriceResponse implements Serializable {
    // https://developer.pagopa.it/send/api/send-api-external-b2b-pa-bundle#/send/api/operations/retrieveNotificationPriceV23
    /**
     * Identificativo Univoco Notifica
     * 25 chars, format: ^[A-Z]{4}-[A-Z]{4}-[A-Z]{4}-[0-9]{6}-[A-Z]{1}-[0-9]{1}$
     */
    private String iun;

    // Total notification cost which does not include paFee and vat
    private Integer partialPrice;

    // Total notification cost including paFee and vat
    private Integer totalPrice;

    // VAT as a percentage (0-100)
    private Integer vat;

    // Cost in eurocent incurred by the sender (0-100)
    private Integer paFee;

    // Refinement date due to expiration of terms
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private ZonedDateTime refinementDate;

    // Refinement date for acknowledgement
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private ZonedDateTime notificationViewDate;

    // Base cost of SeND per notification
    private Integer sendFee;

    // Total cost of postal products
    private Integer analogCost;
}
