package it.gov.pagopa.debtposition.model.payments;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class OrganizationModelQueryBean implements Serializable {

    /**
     * generated serialVersionUID
     */

    private static final long serialVersionUID = -2167998567826059075L;


    private String organizationFiscalCode;

}
