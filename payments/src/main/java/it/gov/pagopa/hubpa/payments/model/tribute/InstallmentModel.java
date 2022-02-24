package it.gov.pagopa.hubpa.payments.model.tribute;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class InstallmentModel implements Serializable{

    private static final long serialVersionUID = 8279315352961365784L;
    
    private LocalDate dueDate;
    private BigDecimal percentagePrimary;
    private BigDecimal percentageSecondary;
}
