package it.gov.pagopa.hubpa.models;

import it.gov.pagopa.hubpa.servicewsdl.TipoIdRendicontazione;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FlowsMessage {

    private String idPA;
    private TipoIdRendicontazione[] flows;
}
