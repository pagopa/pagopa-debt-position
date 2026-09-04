package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.util.CommonUtil;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import org.springframework.data.jpa.domain.Specification;

public class PaymentOptionByAttribute implements Specification<PaymentOption> {

  /**
   * generated serialVersionUID
   */
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

  public PaymentOptionByAttribute(
      LocalDateTime dateFrom,
      LocalDateTime dateTo,
      List<String> segregationCodes) {
    this.paymentPosition = null; // bulk mode
    this.dateFrom = dateFrom;
    this.dateTo = dateTo;
    this.segregationCodes = segregationCodes;
  }

  @Override
  public Predicate toPredicate(
      Root<PaymentOption> root, CriteriaQuery<?> query, CriteriaBuilder cb) {

    List<Predicate> predicates = new ArrayList<>();

    // payment position predicate (only if paymentPosition != null)
    if (this.paymentPosition != null) {
      predicates.add(cb.equal(root.get("paymentPosition"), this.paymentPosition));
    }

    // due date predicate (only if at least one bound is provided)
    if (dateFrom != null && dateTo != null) {
      predicates.add(cb.between(root.get(DUEDATE_FIELD), dateFrom, dateTo));
    } else if (dateFrom != null) {
      predicates.add(cb.greaterThanOrEqualTo(root.get(DUEDATE_FIELD), dateFrom));
    } else if (dateTo != null) {
      predicates.add(cb.lessThanOrEqualTo(root.get(DUEDATE_FIELD), dateTo));
    }

    // segregation codes predicate (only if provided)
    if (segregationCodes != null && !segregationCodes.isEmpty()) {
      List<Predicate> ors = new java.util.ArrayList<>(segregationCodes.size());
      for (String code : segregationCodes) {
        ors.add(
            cb.and(
                cb.greaterThanOrEqualTo(root.get(IUV_FIELD), code),
                cb.lessThan(root.get(IUV_FIELD), CommonUtil.getSegregationCodeEnd(code))
            )
        );
      }
      predicates.add(cb.or(ors.toArray(new Predicate[0])));
    }

    // if nothing was added => always true
    return predicates.isEmpty() ? cb.conjunction() : cb.and(predicates.toArray(new Predicate[0]));
  }
}
