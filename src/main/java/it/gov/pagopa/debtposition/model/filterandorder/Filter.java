package it.gov.pagopa.debtposition.model.filterandorder;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import java.time.LocalDateTime;
import java.util.ArrayList;
import javax.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@Builder
public class Filter {
  @NotNull private String organizationFiscalCode;

  private LocalDateTime dueDateFrom;
  private LocalDateTime dueDateTo;
  private LocalDateTime paymentDateFrom;
  private LocalDateTime paymentDateTo;
  private DebtPositionStatus status;
  private ArrayList<String> segregationCodes;
  private ServiceType serviceType;
}
