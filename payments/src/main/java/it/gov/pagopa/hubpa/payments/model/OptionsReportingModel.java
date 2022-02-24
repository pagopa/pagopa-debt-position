package it.gov.pagopa.hubpa.payments.model;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
public class OptionsReportingModel implements Serializable {

    private static final long serialVersionUID = 27984905799980540L;
    @NotNull
    private String idFlow;
    @NotNull
    private LocalDate dateFlow;
    @NotEmpty
    private List<String> notificationCodes;
}
