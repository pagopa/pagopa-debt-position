package it.gov.pagopa.debtposition.model.pd.response;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentPositionModelBaseResponse implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = 3162169541264601092L;
    
    private String iupd;
    private String organizationFiscalCode;
    private String companyName; // es. Comune di Roma
    private String officeName; // es. Ufficio Tributi
    private LocalDateTime insertedDate;
    private LocalDateTime publishDate;
    private LocalDateTime validityDate;
    private DebtPositionStatus status;
    private LocalDateTime lastUpdatedDate;

    private List<PaymentOptionModelResponse> paymentOption = new ArrayList<>();
}
