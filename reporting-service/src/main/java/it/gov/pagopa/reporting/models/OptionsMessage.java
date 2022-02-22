package it.gov.pagopa.reporting.models;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class OptionsMessage {

    private String idPA;
    private String idFlow;
    private String flowDate;
    private List<PaymentOption> paymentOptions;

}