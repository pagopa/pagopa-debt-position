package it.gov.pagopa.debtposition.model.payments.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OrganizationListModelResponse implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -5630972730850116336L;


    private List<OrganizationModelResponse> add;
    private List<OrganizationModelResponse> delete;

}
