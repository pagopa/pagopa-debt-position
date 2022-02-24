package it.gov.pagopa.hubpa.payments.model;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.List;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@NoArgsConstructor
public class PublishModel implements Serializable {

    private static final long serialVersionUID = 27984905799980540L;
    @NotEmpty
    private List<Long> ids;
    @NotNull
    private LocalDate publishDate;
}
