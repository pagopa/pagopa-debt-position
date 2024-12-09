package it.gov.pagopa.debtposition.model.odp.response;

import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
public class InstallmentModelResponse implements Serializable {

    private String nav;
    private String iuv;
    private String organizationFiscalCode;
    private long amount;
    private String description;
    private LocalDateTime dueDate;
    private LocalDateTime paymentDate;
    private LocalDateTime reportingDate;
    private String paymentMethod;
    private String pspCompany;
    private long fee;
    private long notificationFee;
    private String idReceipt;
    private String idFlowReporting;
    private PaymentOptionStatus status;
    private LocalDateTime lastUpdatedDate;

    private List<InstallmentMetadataModelResponse> installmentMetadata = new ArrayList<>();

    private List<TransferModelResponse> transfer = new ArrayList<>();
}
