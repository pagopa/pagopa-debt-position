package it.gov.pagopa.reporting.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class PaymentOption {

    private String optionId;
    private Integer transferId;

    // retry
    private String retryAction;
}
