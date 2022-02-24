package it.gov.pagopa.payments.validation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.validation.Constraint;
import javax.validation.Payload;

import it.gov.pagopa.payments.validation.implement.IbanCustomValidator;

@Documented
@Constraint(validatedBy = IbanCustomValidator.class)
@Target({ ElementType.METHOD, ElementType.FIELD, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface Iban  {
	String message() default "International Bank Account Number";
	
	Class<?>[] groups() default {};

	Class<? extends Payload>[] payload() default {};
	
}
