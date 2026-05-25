package it.gov.pagopa.debtposition.controller.pd.validator;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Validates that metadata entries do not contain duplicated keys within the same parent item.
 */
@Documented
@Constraint(validatedBy = UniqueMetadataKeysValidator.class)
@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface UniqueMetadataKeys {

  String message() default "metadata keys must be unique";

  Class<?>[] groups() default {};

  Class<? extends Payload>[] payload() default {};
}