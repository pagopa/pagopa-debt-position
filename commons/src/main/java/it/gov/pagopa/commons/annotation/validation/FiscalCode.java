package it.gov.pagopa.commons.annotation.validation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.validation.Constraint;
import javax.validation.Payload;

import it.gov.pagopa.commons.annotation.validation.implement.FiscalCodeValidator;


@Documented
@Constraint(validatedBy = FiscalCodeValidator.class)
@Target({ ElementType.METHOD, ElementType.FIELD, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface FiscalCode  {
	String message() default "codice fiscale non valido";
	
	Class<?>[] groups() default {};

	Class<? extends Payload>[] payload() default {};
	
}
