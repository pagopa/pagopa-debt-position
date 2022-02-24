package it.gov.pagopa.commons.annotation.validation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.validation.Constraint;
import javax.validation.Payload;

import it.gov.pagopa.commons.annotation.validation.implement.IvaCodeValidator;

@Documented
@Constraint(validatedBy = IvaCodeValidator.class)
@Target({ ElementType.METHOD, ElementType.FIELD, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface IvaCode {
	String message() default "codice iva non valido";

	Class<?>[] groups() default {};

	Class<? extends Payload>[] payload() default {};
}
