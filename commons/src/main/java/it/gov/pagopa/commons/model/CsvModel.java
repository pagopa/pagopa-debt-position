package it.gov.pagopa.commons.model;

import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class CsvModel implements Serializable {

    private static final long serialVersionUID = -996346261287121570L;

    private String fileName;
    private List<CsvRowModel> rows;
}
