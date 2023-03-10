package it.gov.pagopa.debtposition.config;

import it.gov.pagopa.debtposition.exception.AppException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.Arrays;
import java.util.Objects;
import java.util.Set;

@Aspect
@Component
public class ExclusiveDatesParam {

    /**
     * This method is triggered when the @ExclusiveParam annotation is used.
     * If the two sets of parameters bound to the given intervals are mutually exclusive
     * the execution of the intercepted method proceeds, otherwise an exception is raised.
     *
     * @throws AppException BAD_REQUEST status is returned
     * @see it.gov.pagopa.debtposition.config.ExclusiveDatesParam
     */
    @Around(value = "@annotation(it.gov.pagopa.debtposition.config.ExclusiveParam)")
    public Object validateExclusiveParam(ProceedingJoinPoint joinPoint) throws Throwable {

        String[] paramsDueDate = {"due_date_to", "due_date_from"};
        String[] paramsPaymentDate = {"payment_date_from", "payment_date_to"};

        Set<String> set = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes()))
                                  .getRequest()
                                  .getParameterMap().keySet();

        boolean bothPresent = Arrays.stream(paramsPaymentDate).anyMatch(set::contains) && Arrays.stream(paramsDueDate).anyMatch(set::contains);

        if (bothPresent) {
            throw new AppException(HttpStatus.BAD_REQUEST,
                    "Exclusive Parameters",
                    "Parameters " +
                            String.join(", ", paramsDueDate) + " and " +
                            String.join(", ", paramsPaymentDate) + " are mutual exclusive");
        }

        return joinPoint.proceed();
    }
}
