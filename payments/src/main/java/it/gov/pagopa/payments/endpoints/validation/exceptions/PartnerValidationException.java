package it.gov.pagopa.payments.endpoints.validation.exceptions;

import it.gov.pagopa.payments.model.PaaErrorEnum;
import lombok.Getter;

@Getter
public class PartnerValidationException extends IllegalArgumentException {

    private static final long serialVersionUID = 1L;

    private final PaaErrorEnum error;

    public PartnerValidationException(PaaErrorEnum error) {
        super(error.getDescription());
        this.error = error;
    }
}
