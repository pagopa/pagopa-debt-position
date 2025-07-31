package it.gov.pagopa.debtposition.controller.pd.validator.v3;

import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class InstallmentsSizeValidator
    implements ConstraintValidator<ValidInstallmentsSize, PaymentPositionModelV3> {

  @Override
  public boolean isValid(PaymentPositionModelV3 value, ConstraintValidatorContext context) {
    // Check installment distribution in payment options by filtering for those with more than 1
    // installment
    long count =
        value.getPaymentOption().stream()
            .filter(po -> po.getInstallments() != null && po.getInstallments().size() > 1)
            .count();

    // N payment options with N Installment is not possible (ie Opzione Rateale Multipla) ->
    // BAD_REQUEST
    return count <= 1;
  }
}
