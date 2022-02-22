package it.gov.pagopa.reporting.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class PaymentOption {

    private String optionId;
    private Integer transferId;

}
