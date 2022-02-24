package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.math.BigDecimal;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class TransfersModel implements Serializable {

    private static final long serialVersionUID = -852982614317823315L;
    
    private BigDecimal partialAmount;
    private String iban;
    private String organizationFiscalCode;
    private String reason;
    private String taxonomy;
    private String postalIban;
    private String postalIbanHolder;
    private String postalAuthCode;
}
