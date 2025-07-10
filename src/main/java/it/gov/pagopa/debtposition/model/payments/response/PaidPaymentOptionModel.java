package it.gov.pagopa.debtposition.model.payments.response;

import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@Schema(name = "PaymentsModelResponse")
public class PaidPaymentOptionModel implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 4163071583836786527L;

  private String nav;
  private String iuv;
  private String organizationFiscalCode;
  private String serviceType;
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
  private long notificationFee;
  private String pspCompany;
  private String idReceipt;
  private String idFlowReporting;
  private PaymentOptionStatus status;
  private LocalDateTime lastUpdatedDate;
  private LocalDateTime lastUpdatedDateNotificationFee;

  private List<PaymentOptionMetadataModelResponse> paymentOptionMetadata = new ArrayList<>();

  private List<TransferModelResponse> transfer = new ArrayList<>();
}
