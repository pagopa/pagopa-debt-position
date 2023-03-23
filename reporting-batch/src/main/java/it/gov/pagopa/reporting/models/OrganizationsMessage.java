package it.gov.pagopa.reporting.models;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrganizationsMessage {

    private String[] idPA;
    private Integer retry;
}
