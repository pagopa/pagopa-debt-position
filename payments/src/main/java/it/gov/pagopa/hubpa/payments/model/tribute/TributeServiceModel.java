package it.gov.pagopa.hubpa.payments.model.tribute;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

import it.gov.pagopa.hubpa.payments.annotation.validation.Iban;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TributeServiceModel implements Serializable {

    private static final long serialVersionUID = -9090133850672769597L;

    private String fiscalCodePrimaryCreditor;
    private String fiscalCodeSecondaryCreditor;
    @Iban
    private String ibanPrimary;
    @Iban
    private String ibanSecondary;
    private String denomination;
    private BigDecimal percentageSecondary;
    private LocalDate dueDateUnique;
    private List<InstallmentModel> installments;
    @Iban
    private String postalIbanPrimary;
    private String postalIbanHolderPrimary;
    private String postalAuthCodePrimary;
    @Iban
    private String postalIbanSecondary;
    private String postalIbanHolderSecondary;
    private String postalAuthCodeSecondary;
}
