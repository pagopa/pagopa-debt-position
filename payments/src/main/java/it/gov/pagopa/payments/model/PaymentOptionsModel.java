package it.gov.pagopa.payments.model;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class PaymentOptionsModel implements Serializable {

    private static final long serialVersionUID = -2235767669718081320L;
    
    private String fiscalCode;
    private BigDecimal amount;
    private LocalDate duoDate;
    private LocalDate retentionDate;
    private Boolean isConclusive;
    private String metadata;
    private Integer status;
    private Boolean allCpp;
    private String idFlowReporting;
    private LocalDate dateReporting;

    private List<TransfersModel> transfers=new ArrayList<>();
}
