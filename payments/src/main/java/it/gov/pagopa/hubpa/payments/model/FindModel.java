package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FindModel implements Serializable {

    private static final long serialVersionUID = 27984805799980540L;

    private FilterModel filters;
    private String fiscalCode;
    private Integer size;
    private Integer page;
}
