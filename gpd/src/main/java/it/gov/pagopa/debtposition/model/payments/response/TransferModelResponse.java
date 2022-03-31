package it.gov.pagopa.debtposition.model.payments.response;

import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@Schema(name = "PaymentsTransferModelResponse")
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
    private LocalDateTime insertedDate;
    private TransferStatus status;
    private LocalDateTime lastUpdatedDate;
}
