package it.gov.pagopa.debtposition.model.filterandorder;

import javax.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import org.springframework.data.domain.Sort;

@Getter
@AllArgsConstructor
@Builder
public class Order {

  @NotNull private OrderType orderBy;

  @NotNull private Sort.Direction ordering;

  @Getter
  @AllArgsConstructor
  public enum PaymentPositionOrder implements OrderType {
    INSERTED_DATE("insertedDate"),
    IUPD("iupd"),
    STATUS("status"),
    COMPANY_NAME("companyName");

    private final String columnName;
  }
}
