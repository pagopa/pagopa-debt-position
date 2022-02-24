package it.gov.pagopa.hubpa.payments.enumeration;

import lombok.Getter;

@Getter
public enum PaymentStatusEnum {
    BOZZA(1),
    PUBBLICATO(2),
    PAGATO(3),
    PAGATO_PARZIALE(4),
    RENDICONTATO_PARZIALE(5),
    RENDICONTATO(6);

    private final Integer status;
    
    PaymentStatusEnum(Integer status) {
        this.status = status;
    }
}
