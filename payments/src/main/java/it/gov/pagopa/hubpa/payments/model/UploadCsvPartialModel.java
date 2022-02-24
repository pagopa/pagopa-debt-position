package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;

import javax.validation.Valid;

import it.gov.pagopa.hubpa.payments.model.csv.CsvModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class UploadCsvPartialModel implements Serializable {

    private static final long serialVersionUID = -996346261287121570L;
    
    @Valid
    private CsvModel csv;
    private String fiscalCodeCreditor;
    private Long jobId;
    
}
