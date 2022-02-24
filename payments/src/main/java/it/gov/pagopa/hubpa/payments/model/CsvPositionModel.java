package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.math.BigDecimal;

import com.opencsv.bean.CsvBindByPosition;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class CsvPositionModel implements Serializable {

    private static final long serialVersionUID = -8752446710569207804L;

    @CsvBindByPosition(position = 0)
    private String name;
    @CsvBindByPosition(position = 1)
    private String surname;
    @CsvBindByPosition(position = 2)
    private String fiscalCode;
    @CsvBindByPosition(position = 3)
    private Integer type;
    @CsvBindByPosition(position = 4)
    private String phone;
    @CsvBindByPosition(position = 5)
    private String address;
    @CsvBindByPosition(position = 6)
    private String number;
    @CsvBindByPosition(position = 7)
    private String area;
    @CsvBindByPosition(position = 8)
    private String province;
    @CsvBindByPosition(position = 9)
    private String cap;
    @CsvBindByPosition(position = 10)
    private String country;
    @CsvBindByPosition(position = 11)
    private String email;
    @CsvBindByPosition(position = 12)
    private String idTenant;
    @CsvBindByPosition(position = 13)
    private BigDecimal amount;
    @CsvBindByPosition(position = 14)
    private String information;
    @CsvBindByPosition(position = 15)
    private String reason;
    
}
