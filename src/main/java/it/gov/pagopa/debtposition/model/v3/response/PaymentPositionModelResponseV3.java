package it.gov.pagopa.debtposition.model.v3.response;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class PaymentPositionModelResponseV3 implements Serializable {

    private String iupd;
    private String organizationFiscalCode;
    private String companyName; // es. Comune di Roma
    private String officeName; // es. Ufficio Tributi
    private LocalDateTime insertedDate;
    private LocalDateTime publishDate;
    private LocalDateTime paymentDate;
    private DebtPositionStatus status;
    private LocalDateTime lastUpdatedDate;

    private List<PaymentOptionModelResponseV3> paymentOption = new ArrayList<>();
}
