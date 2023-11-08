package it.gov.pagopa.debtposition.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class TransferDTO implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = 7001055971430256321L;
    
    private String organizationFiscalCode;
    private String idTransfer;
    private long amount;
    private String remittanceInformation; // causale
    private String category; // taxonomy
    private String iban;
    private String postalIban;
    private TransferStatus status;  
    
    private List<TransferMetadataDTO> transferMetadata = new ArrayList<>();
    
    public void addTransferMetadata(TransferMetadataDTO metadata) {
    	transferMetadata.add(metadata);
    }

    public void removeTransferMetadata(TransferMetadataDTO metadata) {
    	transferMetadata.remove(metadata);
    }
}
