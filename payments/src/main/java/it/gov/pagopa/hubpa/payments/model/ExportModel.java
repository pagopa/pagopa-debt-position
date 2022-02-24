package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class ExportModel implements Serializable {

    private static final long serialVersionUID = 6746208270787311080L;

    private List<Long> ids;
    private Boolean isMailing;
}
