package it.gov.pagopa.hubpa.models;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class OptionsReportingModel {

    private String idFlow;
    private String dateFlow;
    private List<String> notificationCodes;
}
