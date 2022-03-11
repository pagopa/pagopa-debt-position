package it.gov.pagopa.reporting.models;

import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FlowsMessage {

    private String idPA;
    private TipoIdRendicontazione[] flows;
    private Integer retry;
}
