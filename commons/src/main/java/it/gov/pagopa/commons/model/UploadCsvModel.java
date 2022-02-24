package it.gov.pagopa.commons.model;

import java.io.Serializable;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class UploadCsvModel implements Serializable {

    private static final long serialVersionUID = -996346261287121570L;

    private CsvModel csv;
    private String fiscalCodeCreditor;
    private Long jobId;

}
