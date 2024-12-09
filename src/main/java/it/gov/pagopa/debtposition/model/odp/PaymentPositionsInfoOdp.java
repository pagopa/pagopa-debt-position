package it.gov.pagopa.debtposition.model.odp;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import it.gov.pagopa.debtposition.model.PageInfo;
import it.gov.pagopa.debtposition.model.odp.response.PaymentPositionModelResponseOdp;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonIgnoreProperties(ignoreUnknown = true)
public class PaymentPositionsInfoOdp {

    @JsonProperty("payment_position_list")
    @Schema(required = true)
    @NotNull
    @Valid
    private List<PaymentPositionModelResponseOdp> ppBaseResponseList;

    @JsonProperty("page_info")
    @Schema(required = true)
    @NotNull
    @Valid
    private PageInfo pageInfo;

}