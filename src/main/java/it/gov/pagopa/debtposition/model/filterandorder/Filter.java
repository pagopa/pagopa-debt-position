package it.gov.pagopa.debtposition.model.filterandorder;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import java.time.LocalDateTime;
import java.util.ArrayList;

import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import jakarta.validation.constraints.NotNull;
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
  private LocalDateTime paymentDateTimeFrom;
  private LocalDateTime paymentDateTimeTo;
  private DebtPositionStatus status;
  private ArrayList<String> segregationCodes;
  private ServiceType serviceType;
}
