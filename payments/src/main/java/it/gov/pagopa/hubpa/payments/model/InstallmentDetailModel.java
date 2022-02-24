package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class InstallmentDetailModel implements Serializable {

    private static final long serialVersionUID = 1792314106010428313L;
    
    private Boolean isConclusive;
    private LocalDate dueDate;
    private BigDecimal amount;
    private String notificationCode;
    private Integer status;

}
