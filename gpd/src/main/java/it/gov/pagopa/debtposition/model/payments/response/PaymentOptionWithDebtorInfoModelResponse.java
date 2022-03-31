package it.gov.pagopa.debtposition.model.payments.response;

import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@Schema(name = "PaymentsWithDebtorInfoModelResponse")
public class PaymentOptionWithDebtorInfoModelResponse implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = 9129763339251863583L;

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

    private Type type;
    private String fiscalCode;
    private String fullName;
    private String streetName;
    private String civicNumber;
    private String postalCode;
    private String city;
    private String province;
    private String region;
    private String country;
    private String email;
    private String phone;
    private String companyName;
    private String officeName;
    private DebtPositionStatus debtPositionStatus;


    private List<TransferModelResponse> transfer = new ArrayList<>();
}
