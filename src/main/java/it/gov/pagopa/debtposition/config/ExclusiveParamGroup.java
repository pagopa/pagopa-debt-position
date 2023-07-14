package it.gov.pagopa.debtposition.config;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation indicates that parameters should be mutually exclusive.
 *
 * @see ExclusiveParamAspect
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface ExclusiveParamGroup {
    /**
     * Use Request name
     *
     * @return permitted request params, mutual exclusive with secondGroup
     */
    String[] firstGroup();

    /**
     * Use Request name
     *
     * @return permitted request params, mutual exclusive with firstGroup
     */
    String[] secondGroup();

}
