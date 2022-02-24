package it.gov.pagopa.payments.model;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;

@Data
@NoArgsConstructor
public class PaymentsModelResponse implements Serializable {

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

    private List<PaymentsTransferModelResponse> transfer;
}
