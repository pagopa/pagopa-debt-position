package it.gov.pagopa.hubpa.payments.enumeration;

import lombok.Getter;

@Getter
public enum PaymentOptionStatusEnum {
    PAGATO(1),
    NON_PAGATO(2),
    RENDICONTATO(3);
    
    private final Integer status;
    
    PaymentOptionStatusEnum(Integer status) {
        this.status = status;
    }
}
