package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;

import it.gov.pagopa.hubpa.payments.model.csv.CsvModel;
import it.gov.pagopa.hubpa.payments.model.tribute.TributeServiceModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class UploadCsvModel implements Serializable {

    private static final long serialVersionUID = -996346261287121570L;
    
    private CsvModel csv;
    private TributeServiceModel tributeService;
    private Long jobId;
    
}
