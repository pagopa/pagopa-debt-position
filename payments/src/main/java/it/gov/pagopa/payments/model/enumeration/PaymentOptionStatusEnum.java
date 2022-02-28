package it.gov.pagopa.payments.model.enumeration;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PaymentOptionStatusEnum {
    PO_UNPAID, PO_PAID, PO_PARTIALLY_REPORTED, PO_REPORTED

}
