package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FindResponseModel implements Serializable {

    private static final long serialVersionUID = -589697835054443484L;
    
    private List<PaymentMinimalModel> payments;
    private Integer currentPage;
    private Long totalItems;
    private Integer totalPages;
}
