package it.gov.pagopa.reporting.models;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class Organizations {

    private List<Organization> add;
    private List<Organization> delete;

}
