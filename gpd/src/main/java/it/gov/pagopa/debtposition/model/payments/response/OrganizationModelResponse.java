package it.gov.pagopa.debtposition.model.payments.response;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
public class OrganizationModelResponse implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = 2258263487093248596L;

    private String organizationFiscalCode;

}
