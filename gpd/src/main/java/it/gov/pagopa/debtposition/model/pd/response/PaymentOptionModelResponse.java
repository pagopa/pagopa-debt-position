package it.gov.pagopa.debtposition.model.pd.response;

import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class PaymentOptionModelResponse implements Serializable {

    /**
     * generated serialVersionUID
     */

    private static final long serialVersionUID = 4163071583836786527L;

    private String iuv;
    private String organizationFiscalCode;
    private long amount;
    private String description;
    private Boolean isPartialPayment;
    private LocalDateTime dueDate;
    private LocalDateTime retentionDate;
    private LocalDateTime paymentDate;
    private LocalDateTime reportingDate;
    private LocalDateTime insertedDate;
    private String paymentMethod;
    private long fee;
    private String pspCompany;
    private String idReceipt;
    private String idFlowReporting;
    private PaymentOptionStatus status;
    private LocalDateTime lastUpdatedDate;

    private List<TransferModelResponse> transfer = new ArrayList<>();
}
