package it.gov.pagopa.payments.model;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;

@Data
@NoArgsConstructor
public class PaymentsTransferModelResponse implements Serializable {

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
