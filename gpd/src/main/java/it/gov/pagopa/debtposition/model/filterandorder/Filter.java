package it.gov.pagopa.debtposition.model.filterandorder;

import java.time.LocalDateTime;

import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

@Getter
@AllArgsConstructor
@Builder
public class Filter {
    @NotNull
    private String organizationFiscalCode;
    
    private LocalDateTime dueDateFrom;
    private LocalDateTime dueDateTo;
}
