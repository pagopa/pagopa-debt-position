package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.time.LocalDate;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class PaymentMinimalModel implements Serializable {

    private static final long serialVersionUID = -6472800307334591261L;
    
    private Long id;
    private String fiscalCode;
    private String name;
    private String surname;
    private LocalDate date;
    private Integer status;
    private Boolean isDuplicated;
}
