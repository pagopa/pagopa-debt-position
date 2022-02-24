package it.gov.pagopa.hubpa.payments.enumeration;

import lombok.Getter;

@Getter
public enum JobStatusEnum {
    IN_ATTESA(1),
    PARZIALE(2),
    FALLITO(3),
    SUCCESSO(4);
    
    private final Integer status;
    
    JobStatusEnum(Integer status) {
        this.status = status;
    }
}
