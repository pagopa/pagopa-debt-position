package it.gov.pagopa.debtposition.model.v3.response;

import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import lombok.Data;
import lombok.NoArgsConstructor;

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
  private InstallmentStatus status;
  private LocalDateTime lastUpdatedDate;

  private List<InstallmentMetadataModel> installmentMetadata = new ArrayList<>();

  private List<TransferModelResponse> transfer = new ArrayList<>();
}
