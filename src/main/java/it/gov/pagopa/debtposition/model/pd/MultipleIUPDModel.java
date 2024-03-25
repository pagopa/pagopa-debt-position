package it.gov.pagopa.debtposition.model.pd;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;
import java.util.List;

@Data
@NoArgsConstructor
public class MultipleIUPDModel {

    @NotEmpty
    @Size(max = 100)
    private List<String> paymentPositionIUPDs;
}
