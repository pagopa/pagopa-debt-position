package it.gov.pagopa.debtposition.dto;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentPositionDTO implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = 6990851241801808964L;
    
    private String iupd;
    private String organizationFiscalCode;
    
    // Debtor properties
    private Type type;
    private String fiscalCode;
    private String fullName;
    private String streetName;
    private String civicNumber;
    private String postalCode;
    private String city;
    private String province;
    private String region;
    private String country;
    private String email;
    private String phone;
    
    // Payment Position properties
    private String companyName; // es. Comune di Roma
    private String officeName; // es. Ufficio Tributi
    private LocalDateTime insertedDate;
    private LocalDateTime publishDate;
    private LocalDateTime validityDate;
    private Boolean switchToExpired;
    private DebtPositionStatus status;
    private LocalDateTime paymentDate;

    private List<PaymentOptionDTO> paymentOption = new ArrayList<>();
    
    public void addPaymentOptions(PaymentOptionDTO paymentOpt) {
        paymentOption.add(paymentOpt);
    }

    public void removePaymentOptions(PaymentOptionDTO paymentOpt) {
        paymentOption.remove(paymentOpt);
    }
}
