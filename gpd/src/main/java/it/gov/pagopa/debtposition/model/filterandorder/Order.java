package it.gov.pagopa.debtposition.model.filterandorder;

import javax.validation.constraints.NotNull;

import org.springframework.data.domain.Sort;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

@Getter
@AllArgsConstructor
@Builder
public class Order {

    @NotNull
    private OrderType orderBy;

    @NotNull
    private Sort.Direction ordering;


    @Getter
    @AllArgsConstructor
    public enum PaymentPositionOrder implements OrderType {
        IUPD("iupd"),
        STATUS("status"),
        COMPANY_NAME("companyName");

        private final String columnName;

    }
}
