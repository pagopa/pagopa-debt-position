package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.util.CommonUtil;
import java.time.LocalDateTime;
import java.util.List;
import jakarta.persistence.criteria.*;
import org.springframework.data.jpa.domain.Specification;

public class PaymentOptionByAttribute implements Specification<PaymentOption> {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 6534338388239897792L;

  private static final String DUEDATE_FIELD = "dueDate";
  private static final String IUV_FIELD = "iuv";

  private LocalDateTime dateFrom;
  private LocalDateTime dateTo;
  private List<String> segregationCodes;
  private PaymentPosition paymentPosition;

  public PaymentOptionByAttribute(
      PaymentPosition paymentPosition,
      LocalDateTime dateFrom,
      LocalDateTime dateTo,
      List<String> segregationCodes) {
    this.paymentPosition = paymentPosition;
    this.dateFrom = dateFrom;
    this.dateTo = dateTo;
    this.segregationCodes = segregationCodes;
  }

  public Predicate toPredicate( // TODO SWITCH TO INSTALLMENT
      Root<PaymentOption> root, CriteriaQuery<?> query, CriteriaBuilder cb) {

    Predicate paymentPositionIdPredicate =
        cb.equal(root.get("paymentPosition"), this.paymentPosition);
    Predicate dueDatePredicate = cb.isTrue(cb.literal(true));
    Predicate segregationCodesPredicate = cb.isTrue(cb.literal(false));

    // due date predicate
    if (dateFrom != null && dateTo == null) {
      dueDatePredicate = cb.greaterThanOrEqualTo(root.get(DUEDATE_FIELD), dateFrom);
    } else if (dateFrom == null && dateTo != null) {
      dueDatePredicate = cb.lessThanOrEqualTo(root.get(DUEDATE_FIELD), dateTo);
    }
    // The execution proceeds on this branch in only 2 cases: dateFrom and dateTo equal null or both
    // different from null,
    // to check the last case just apply the condition on one of the two dates
    else if (dateFrom != null) {
      dueDatePredicate = cb.between(root.get(DUEDATE_FIELD), dateFrom, dateTo);
    }

    // segregation code predicate
    if (segregationCodes != null && !segregationCodes.isEmpty()) {
      for (String segregationCode : segregationCodes) {
        segregationCodesPredicate =
            cb.or(
                segregationCodesPredicate,
                cb.between(
                    root.get(IUV_FIELD),
                    segregationCode,
                    CommonUtil.getSegregationCodeEnd(segregationCode)));
      }
    } else {
      segregationCodesPredicate = cb.isTrue(cb.literal(true));
    }

    return cb.and(paymentPositionIdPredicate, dueDatePredicate, segregationCodesPredicate);
  }
}
