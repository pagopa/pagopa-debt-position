package it.gov.pagopa.debtposition.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
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
    private Stamp stamp;
    private TransferStatus status;  
    
    private List<TransferMetadataDTO> transferMetadata = new ArrayList<>();

    public TransferDTO(String fc, String id, long amount, String remittance, String category, String iban, String postalIban, Stamp stamp, TransferStatus status) {
        this.organizationFiscalCode = fc;
        this.idTransfer = id;
        this.amount = amount;
        this.remittanceInformation = remittance;
        this.category = category;
        this.iban = iban;
        this.postalIban = postalIban;
        this.stamp = stamp;
        this.status = status;
    }

    public void addTransferMetadata(TransferMetadataDTO metadata) {
    	transferMetadata.add(metadata);
    }

    public void removeTransferMetadata(TransferMetadataDTO metadata) {
    	transferMetadata.remove(metadata);
    }
}
