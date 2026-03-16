package it.gov.pagopa.debtposition.config;

import it.gov.pagopa.debtposition.exception.AppException;
import java.util.Arrays;
import java.util.Set;
import jakarta.servlet.http.HttpServletRequest;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class ExclusiveParamAspect {

  @Autowired private HttpServletRequest request;

  /**
   * This method is triggered when the @ExclusiveParam annotation is used. If the two sets of
   * parameters bound to the given intervals are mutually exclusive the execution of the intercepted
   * method proceeds, otherwise an exception is raised.
   *
   * @throws AppException BAD_REQUEST status is returned
   * @see ExclusiveParamAspect
   */
  @Around(value = "@annotation(annotation)")
  public Object validateExclusiveParam(
      ProceedingJoinPoint joinPoint, ExclusiveParamGroup annotation) throws Throwable {

    String[] paramsDueDate = annotation.firstGroup();
    String[] paramsPaymentDate = annotation.secondGroup();
    String[] paramsPaymentDateTime = annotation.thirdGroup();

    Set<String> set = request.getParameterMap().keySet();

    // If two of the three couple of parameters are present, we return bad request
    boolean multiplePresent = Arrays.stream(paramsPaymentDate).anyMatch(set::contains) ?
            Arrays.stream(paramsDueDate).anyMatch(set::contains) || Arrays.stream(paramsPaymentDateTime).anyMatch(set::contains) :
            Arrays.stream(paramsDueDate).anyMatch(set::contains) && Arrays.stream(paramsPaymentDateTime).anyMatch(set::contains);

    if (multiplePresent) {
      throw new AppException(
          HttpStatus.BAD_REQUEST,
          "Exclusive Parameters",
          "Parameters "
              + String.join(", ", paramsDueDate)
              + " and "
              + String.join(", ", paramsPaymentDate)
              + "and"
              + String.join(", ", paramsPaymentDateTime)
              + " are mutual exclusive");
    }

    return joinPoint.proceed();
  }
}
