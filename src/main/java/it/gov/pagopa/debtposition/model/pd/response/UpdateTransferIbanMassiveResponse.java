package it.gov.pagopa.debtposition.model.pd.response;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Builder
@Getter
@Setter
public class UpdateTransferIbanMassiveResponse {

    String description;
    int updatedTransfers;
}
