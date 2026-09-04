package it.gov.pagopa.debtposition.dto;

import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class PaymentOptionDTO implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -4792852695535892332L;

  private String nav;
  private String iuv;
  private String organizationFiscalCode;
  private long amount;
  private String description;
  private Boolean isPartialPayment;
  private LocalDateTime dueDate;
  private LocalDateTime retentionDate;
  private LocalDateTime paymentDate;
  private LocalDateTime reportingDate;
  private LocalDateTime validityDate;
  private String paymentMethod;
  private long fee;
  private long notificationFee; // needed for testing if ignored in unmarshalling
  private String pspCompany;
  private String idReceipt;
  private String idFlowReporting;
  private PaymentOptionStatus status;
  private Boolean switchToExpired = false;

  private List<PaymentOptionMetadataDTO> paymentOptionMetadata = new ArrayList<>();

  public void addPaymentOptionMetadata(PaymentOptionMetadataDTO metadata) {
    paymentOptionMetadata.add(metadata);
  }

  public void removePaymentOptionMetadata(PaymentOptionMetadataDTO metadata) {
    paymentOptionMetadata.remove(metadata);
  }

  private List<TransferDTO> transfer = new ArrayList<>();

  public void addTransfers(TransferDTO trans) {
    transfer.add(trans);
  }

  public void removeTransfers(TransferDTO trans) {
    transfer.remove(trans);
  }
}
