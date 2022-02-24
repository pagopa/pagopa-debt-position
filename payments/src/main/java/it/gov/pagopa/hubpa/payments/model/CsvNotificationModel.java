package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;

import com.opencsv.bean.CsvBindByPosition;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class CsvNotificationModel implements Serializable {

    private static final long serialVersionUID = 2347328708266160424L;
    
    @CsvBindByPosition(position = 0)
    private String fileName;
    @CsvBindByPosition(position = 1)
    private String mailingType;
    @CsvBindByPosition(position = 2)
    private String debitorName;
    @CsvBindByPosition(position = 3)
    private String debitorAddress;
    @CsvBindByPosition(position = 4)
    private String debitorCap;
    @CsvBindByPosition(position = 5)
    private String debitorLocality;
    @CsvBindByPosition(position = 6)
    private String debitorProvince;
    @CsvBindByPosition(position = 7)
    private String debitorNation;
    @CsvBindByPosition(position = 8)
    private String creditorName;
    @CsvBindByPosition(position = 9)
    private String creditorAddress;
    @CsvBindByPosition(position = 10)
    private String creditorCap;
    @CsvBindByPosition(position = 11)
    private String creditorLocality;
    @CsvBindByPosition(position = 12)
    private String creditorProvince;
    @CsvBindByPosition(position = 13)
    private String creditorNation;
    @CsvBindByPosition(position = 14)
    private Integer frontages;
    @CsvBindByPosition(position = 15)
    private Integer pagePostalFrom;
    @CsvBindByPosition(position = 16)
    private Integer pagePostalTo;
    @CsvBindByPosition(position = 17)
    private String idDoc1;
    @CsvBindByPosition(position = 18)
    private String idDoc2;
    @CsvBindByPosition(position = 19)
    private String idDoc3;
    @CsvBindByPosition(position = 20)
    private String idDoc4;
    @CsvBindByPosition(position = 21)
    private String idDoc5;
    @CsvBindByPosition(position = 22)
    private String idDoc6;
    @CsvBindByPosition(position = 23)
    private String idDoc7;
    @CsvBindByPosition(position = 24)
    private String idDoc8;
    @CsvBindByPosition(position = 25)
    private String attachment1;
    @CsvBindByPosition(position = 26)
    private String attachmentWeight1;
    @CsvBindByPosition(position = 27)
    private String attachment2;
    @CsvBindByPosition(position = 28)
    private String attachmentWeight2;
    @CsvBindByPosition(position = 29)
    private String printMode;
    @CsvBindByPosition(position = 30)
    private String printType;
    @CsvBindByPosition(position = 31)
    private String billingType;
    @CsvBindByPosition(position = 32)
    private String costCenter;
    
}
