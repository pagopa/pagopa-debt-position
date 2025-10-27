package it.gov.pagopa.debtposition.controller.pd.validator.v3;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class InstallmentsSizeValidator
        implements ConstraintValidator<ValidInstallmentsSize, PaymentPositionModelV3> {

  @Override
  public boolean isValid(PaymentPositionModelV3 value, ConstraintValidatorContext ctx) {
    // TODO VERIFY
    if (value == null) return true; // nothing to validate
    var options = value.getPaymentOption();
    if (options == null) return true;

    ctx.disableDefaultConstraintViolation();

    return validateGlobalIuvUniqueness(options, ctx);
  }

  private boolean validateGlobalIuvUniqueness(List<PaymentOptionModelV3> options,
                                              ConstraintValidatorContext ctx) {
    // Global uniqueness of IUVs across all installments     // TODO VERIFY
    boolean ok = true;
    Set<String> seenIuv = new HashSet<>();

    for (int i = 0; i < options.size(); i++) {
      var po = options.get(i);
      if (po == null || po.getInstallments() == null) continue;

      int j = 0;
      for (var inst : po.getInstallments()) {
        if (inst != null) {
          String iuv = inst.getIuv();
          if (iuv != null && !seenIuv.add(iuv)) {
            addViolation(ctx, "duplicate IUV at paymentOption[" + i + "].installments[" + j + "]");
            ok = false;
          }
        }
        j++;
      }
    }
    return ok;
  }

  private void addViolation(ConstraintValidatorContext ctx, String msg) {
    ctx.buildConstraintViolationWithTemplate(msg).addConstraintViolation();
  }

}