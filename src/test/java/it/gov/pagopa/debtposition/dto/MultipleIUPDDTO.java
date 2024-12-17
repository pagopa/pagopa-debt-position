package it.gov.pagopa.debtposition.dto;

import java.util.List;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class MultipleIUPDDTO {

    @NotEmpty
    @Size(max = 100)
    private List<String> paymentPositionIUPDs;
}
