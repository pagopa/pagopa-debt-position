package it.gov.pagopa.debtposition.controller.pd.validator;

import java.util.List;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import org.hibernate.validator.constraintvalidation.HibernateConstraintValidatorContext;
import org.springframework.beans.factory.annotation.Value;

import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;

public class ValidMaxDebtPositions implements ConstraintValidator<ValidDebtPositionsSize, List<PaymentPositionModel>> {

	private int minDebtPositions = 1;
	@Value("${max.massive.debt.positions: 100}")
	private int maxDebtPositions;

	@Override
	public boolean isValid(List<PaymentPositionModel> value, ConstraintValidatorContext context) {
		// leave null-checking to @NotNull
		if (value == null) {
			return true;
		}
		formatMessage(context);
		return value.size() <= maxDebtPositions;
	}

	private void formatMessage(ConstraintValidatorContext context) {
		HibernateConstraintValidatorContext hibernateContext = context
				.unwrap(HibernateConstraintValidatorContext.class);
		hibernateContext.addMessageParameter("min", this.minDebtPositions)
		.addMessageParameter("max",this.maxDebtPositions);
		context.disableDefaultConstraintViolation();
		context.buildConstraintViolationWithTemplate(context.getDefaultConstraintMessageTemplate())
		.addConstraintViolation();
	}
}
