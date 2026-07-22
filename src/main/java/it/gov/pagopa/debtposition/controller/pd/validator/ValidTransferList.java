package it.gov.pagopa.debtposition.controller.pd.validator;

import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.ElementType.TYPE_USE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import jakarta.validation.constraints.Size;

@Target({METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Constraint(validatedBy = ValidTransferListValidator.class)
public @interface ValidTransferList {

  String message() default "{javax.validation.constraints.Size.message}";

  Class<?>[] groups() default {};

  Class<? extends Payload>[] payload() default {};

  public static int DEFAULT_MIN_SIZE = 1;
  public static int DEFAULT_MAX_SIZE = 5;

  /**
   * @return size the element must be higher or equal to
   */
  int min() default DEFAULT_MIN_SIZE;

  /**
   * @return size the element must be lower or equal to
   */
  int max() default DEFAULT_MAX_SIZE;

}
