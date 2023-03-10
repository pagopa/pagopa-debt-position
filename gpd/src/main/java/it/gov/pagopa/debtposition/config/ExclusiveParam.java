package it.gov.pagopa.debtposition.config;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation indicates that parameters related to date intervals should be mutually exclusive.
 *
 * @see it.gov.pagopa.debtposition.config.ExclusiveDatesParam
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface ExclusiveParam {
}