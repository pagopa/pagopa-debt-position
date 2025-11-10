package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.util.CommonUtil;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import jakarta.persistence.criteria.*;
import org.springframework.data.jpa.domain.Specification;

public class PaymentPositionByOptionsAttribute implements Specification<PaymentPosition> {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 6534338388239897792L;

  private static final String PAYMENT_OPT_JOIN = "paymentOption";
  private static final String DUEDATE_FIELD = "dueDate";
  private static final String IUV_FIELD = "iuv";

  private static final String PAYMENT_PLAN_ID_FIELD = "paymentPlanId";
  private static final String IS_PARTIAL_PAYMENT_FIELD = "isPartialPayment";

  private boolean filterMultiInstalments;
  private LocalDateTime dateFrom;
  private LocalDateTime dateTo;
  private List<String> segregationCodes;

  public PaymentPositionByOptionsAttribute(
      LocalDateTime dateFrom, LocalDateTime dateTo, List<String> segregationCodes, boolean filterOutMultiInstalments) {
    this.dateFrom = dateFrom;
    this.dateTo = dateTo;
    this.segregationCodes = segregationCodes;
    this.filterMultiInstalments = filterOutMultiInstalments;
  }

  public Predicate toPredicate(
      Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
      query.distinct(true);

    Join<?, ?> ppOptionsJoin = root.join(PAYMENT_OPT_JOIN, JoinType.INNER);
    List<Predicate> predicates = new ArrayList<>();

    Predicate dueDatePredicate = cb.isTrue(cb.literal(true));
    Predicate segregationCodesPredicate = cb.isTrue(cb.literal(false));

    // due date predicate
    if (dateFrom != null && dateTo == null) {
      dueDatePredicate = cb.greaterThanOrEqualTo(ppOptionsJoin.get(DUEDATE_FIELD), dateFrom);
    } else if (dateFrom == null && dateTo != null) {
      dueDatePredicate = cb.lessThanOrEqualTo(ppOptionsJoin.get(DUEDATE_FIELD), dateTo);
    }
    // The execution proceeds on this branch in only 2 cases: dateFrom and dateTo equal null or both
    // different from null,
    // to check the last case just apply the condition on one of the two dates
    else if (dateFrom != null) {
      dueDatePredicate = cb.between(ppOptionsJoin.get(DUEDATE_FIELD), dateFrom, dateTo);
    }
    predicates.add(dueDatePredicate);

    // segregation code predicate
    if (segregationCodes != null && !segregationCodes.isEmpty()) {
      for (String segregationCode : segregationCodes) {
        segregationCodesPredicate =
            cb.or(
                segregationCodesPredicate,
                cb.between(
                    ppOptionsJoin.get(IUV_FIELD),
                    segregationCode,
                    CommonUtil.getSegregationCodeEnd(segregationCode)));
      }
    } else {
      segregationCodesPredicate = cb.isTrue(cb.literal(true));
    }
    predicates.add(segregationCodesPredicate);

    // Skip multi-installments
    if (this.filterMultiInstalments) {
          // SQL:
          // ... AND (
          //     SELECT COUNT(DISTINCT po.payment_plan_id)
          //     FROM payment_option po
          //     WHERE po.payment_position_id = pp.id
          //       AND po.is_partial_payment = true
          // )
      Subquery<Long> subquery = query.subquery(Long.class);
      Root<PaymentPosition> subRoot = subquery.correlate(root);
      Join<?, ?> subOptionsJoin = subRoot.join(PAYMENT_OPT_JOIN);

      Predicate partialPaymentPredicate = cb.isTrue(
              subOptionsJoin.get(IS_PARTIAL_PAYMENT_FIELD)
      );
      subquery.select(cb.countDistinct(subOptionsJoin.get(PAYMENT_PLAN_ID_FIELD)))
              .where(partialPaymentPredicate);
      Predicate planIdCountPredicate = cb.lessThanOrEqualTo(subquery, 1L);
      predicates.add(planIdCountPredicate);
    }

    return cb.and(predicates.toArray(new Predicate[0]));
  }
}
