package it.gov.pagopa.debtposition.dto;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentOptionDTO implements Serializable {

   
    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -4792852695535892332L;
    
    private String iuv;
    private String organizationFiscalCode;
    private long amount;
    private String description;
    private Boolean isPartialPayment;
    private LocalDateTime dueDate;
    private LocalDateTime retentionDate;
    private LocalDateTime paymentDate;
    private LocalDateTime reportingDate;
    private String paymentMethod;
    private long fee;
    private String pspCompany;
    private String idReceipt;
    private String idFlowReporting;
    private PaymentOptionStatus status;   

    private List<TransferDTO> transfer = new ArrayList<>();
    
    public void addTransfers(TransferDTO trans) {
        transfer.add(trans);
    }

    public void removeTransfers(TransferDTO trans) {
        transfer.remove(trans);
    }
}
