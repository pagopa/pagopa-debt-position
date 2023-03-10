package it.gov.pagopa.debtposition.config;

import it.gov.pagopa.debtposition.exception.AppException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Set;

@Aspect
@Component
public class ExclusiveParamAspect {

    @Autowired
    private HttpServletRequest request;


    /**
     * This method is triggered when the @ExclusiveParam annotation is used.
     * If the two sets of parameters bound to the given intervals are mutually exclusive
     * the execution of the intercepted method proceeds, otherwise an exception is raised.
     *
     * @throws AppException BAD_REQUEST status is returned
     * @see ExclusiveParamAspect
     */
    @Around(value = "@annotation(annotation)")
    public Object validateExclusiveParam(ProceedingJoinPoint joinPoint, ExclusiveParamGroup annotation) throws Throwable {

        String[] paramsDueDate = annotation.firstGroup();
        String[] paramsPaymentDate = annotation.secondGroup();

        Set<String> set = request.getParameterMap().keySet();

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
