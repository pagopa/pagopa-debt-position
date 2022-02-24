package it.gov.pagopa.commons.model;

import java.io.Serializable;
import java.math.BigDecimal;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class CsvRowModel implements Serializable {

    private static final long serialVersionUID = -8752446710569207804L;
    private String name;
    private String surname;
    private String fiscalCode;
    private Integer type;
    private String phone;
    private String address;
    private String number;
    private String area;
    private String province;
    private String cap;
    private String country;
    private String email;
    private String idTenant;
    private BigDecimal amount;
    private String reason;

}
