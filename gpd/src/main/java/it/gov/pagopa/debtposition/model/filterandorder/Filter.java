package it.gov.pagopa.debtposition.model.filterandorder;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;

@Getter
@AllArgsConstructor
@Builder
public class Filter {
    @NotNull
    private String organizationFiscalCode;

    private LocalDateTime dueDateFrom;
    private LocalDateTime dueDateTo;
}
