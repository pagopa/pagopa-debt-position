package it.gov.pagopa.debtposition.model.pd;

import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.List;

@Data
@NoArgsConstructor
public class MultipleIUPDModel {

    @NotEmpty
    @Size(max = 20)
    @NotNull
    private List<String> paymentPositionIUPDs;
}
