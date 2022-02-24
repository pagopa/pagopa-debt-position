package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class PaymentPositionModel implements Serializable {

    private static final long serialVersionUID = -2851461971637130474L;

    private String organizationFiscalCode;
    private String companyName;
    private String officeName;
    private Integer status;
    private String description;
    private Long jobId;
    private BigDecimal amount;
    private Integer totalOptions;
    private Integer paidOptions;
    private Integer reportedOptions;

    private List<PaymentOptionsModel> paymentOptions=new ArrayList<>();
}
