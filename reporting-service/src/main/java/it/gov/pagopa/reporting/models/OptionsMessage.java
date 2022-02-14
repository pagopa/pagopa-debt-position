package it.gov.pagopa.hubpa.models;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class OptionsMessage {

    private String idFlow;
    private String dateFlow;
    private String[] iuvs;
}