package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class PaymentPositionDetailModel implements Serializable {

    private static final long serialVersionUID = -2529034821959003847L;

    private String nominative;
    private String fiscalCode;
    private String addressLine1;
    private String addressLine2;
    private String description;
    private Integer status;
    private LocalDate publishDate;
    @Setter(value = AccessLevel.NONE)
    private List<InstallmentDetailModel> installments=new ArrayList<>();
}
