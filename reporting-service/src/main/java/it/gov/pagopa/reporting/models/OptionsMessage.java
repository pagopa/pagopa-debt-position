package it.gov.pagopa.reporting.models;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class OptionsMessage {

    private String idPA;
    private String idFlow;
    private String flowDate;
    private List<PaymentOption> paymentOptions;

    // retry
    private Integer retryCount;

}
