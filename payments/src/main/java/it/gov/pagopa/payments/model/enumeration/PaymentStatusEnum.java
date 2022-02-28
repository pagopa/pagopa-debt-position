package it.gov.pagopa.payments.model.enumeration;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PaymentStatusEnum {
    DRAFT, PUBLISHED, VALID, INVALID, EXPIRED, PARTIALLY_PAID, PAID, REPORTED

}
