package it.gov.pagopa.hubpa.payments.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class DebitorModel implements Serializable {

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
  
    private List<PaymentPositionModel> paymentPosition=new ArrayList<>();
}
