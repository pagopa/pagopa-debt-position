package it.gov.pagopa.reporting.model;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class Flow {

    String flowId;
    String flowDate;
}
