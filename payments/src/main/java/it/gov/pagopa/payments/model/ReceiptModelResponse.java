package it.gov.pagopa.payments.model;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
public class ReceiptModelResponse {

    private String organizationFiscalCode;
    private String iuv;
    private String debtorFiscalCode;
}
