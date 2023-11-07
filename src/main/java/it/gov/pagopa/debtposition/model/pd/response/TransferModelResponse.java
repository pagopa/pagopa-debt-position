package it.gov.pagopa.debtposition.model.pd.response;

import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class TransferModelResponse implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -8466280136220999882L;

    private String organizationFiscalCode;
    private String idTransfer;
    private long amount;
    private String remittanceInformation; // causale
    private String category; // taxonomy
    private String iban;
    private String postalIban;
    private Stamp stamp;
    private LocalDateTime insertedDate;
    private TransferStatus status;
    private LocalDateTime lastUpdatedDate;
    
    private List<TransferMetadataModelResponse> transferMetadata = new ArrayList<>();
}
