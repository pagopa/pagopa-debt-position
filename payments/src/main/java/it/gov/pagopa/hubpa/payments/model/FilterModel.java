package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.time.LocalDate;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FilterModel implements Serializable {

    private static final long serialVersionUID = 7161805092515646292L;

    private LocalDate dateFrom;
    private LocalDate dateTo;
    private Integer status;
    private String textSearch;
}
