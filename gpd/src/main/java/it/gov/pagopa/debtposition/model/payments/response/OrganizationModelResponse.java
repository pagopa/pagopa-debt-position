package it.gov.pagopa.debtposition.model.payments.response;

import java.io.Serializable;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class OrganizationModelResponse implements Serializable {
 
	/**
     * generated serialVersionUID
     */
	private static final long serialVersionUID = 2258263487093248596L;
	
    private String organizationFiscalCode;
    
}
