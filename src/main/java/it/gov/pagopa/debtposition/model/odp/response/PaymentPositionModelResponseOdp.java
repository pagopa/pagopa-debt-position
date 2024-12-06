package it.gov.pagopa.debtposition.model.odp.response;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionModelResponse;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class PaymentPositionModelResponseOdp implements Serializable {

    private String iupd;
    private String organizationFiscalCode;
    private String companyName; // es. Comune di Roma
    private String officeName; // es. Ufficio Tributi
    private LocalDateTime insertedDate;
    private LocalDateTime publishDate;
    private LocalDateTime validityDate;
    private LocalDateTime paymentDate;
    private DebtPositionStatus status;
    private LocalDateTime lastUpdatedDate;

    private List<PaymentOptionModelResponseOdp> paymentOption = new ArrayList<>();
}
