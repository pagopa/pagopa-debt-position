package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentOption;
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

 private static final String PO_PAYMENT_POSITION_FIELD = "paymentPosition";
 private static final String DUEDATE_FIELD = "dueDate";
 private static final String IUV_FIELD = "iuv";

 private final LocalDateTime dateFrom;
 private final LocalDateTime dateTo;
 private final List<String> segregationCodes;

 public PaymentPositionByOptionsAttribute(
     LocalDateTime dateFrom, LocalDateTime dateTo, List<String> segregationCodes) {
   this.dateFrom = dateFrom;
   this.dateTo = dateTo;
   this.segregationCodes = segregationCodes;
 }

 @Override
 public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {

   boolean hasDueFilter = (dateFrom != null || dateTo != null);
   boolean hasSegFilter = (segregationCodes != null && !segregationCodes.isEmpty());

   // If there are no filters on PaymentOption, no join/subquery is forced and the predicate is always true
   if (!hasDueFilter && !hasSegFilter) {
     return cb.conjunction();
   }

   // EXISTS (select 1 from PaymentOption po where po.paymentPosition = root and ...)
   Subquery<Integer> sq = query.subquery(Integer.class);
   Root<PaymentOption> po = sq.from(PaymentOption.class);

   List<Predicate> predicates = new ArrayList<>();

   // correlation: po.paymentPosition = pp(root)
   predicates.add(cb.equal(po.get(PO_PAYMENT_POSITION_FIELD), root));

   // two dates predicated (as subquery)
   if (dateFrom != null && dateTo == null) {
     predicates.add(cb.greaterThanOrEqualTo(po.get(DUEDATE_FIELD), dateFrom));
   } else if (dateFrom == null && dateTo != null) {
     predicates.add(cb.lessThanOrEqualTo(po.get(DUEDATE_FIELD), dateTo));
   } else if (dateFrom != null) {
     // dateFrom != null && dateTo != null
     predicates.add(cb.between(po.get(DUEDATE_FIELD), dateFrom, dateTo));
   }

   // segregation code predicate
   if (hasSegFilter) {
     Predicate seg = cb.disjunction(); // false
     for (String segregationCode : segregationCodes) {
       seg =
           cb.or(
               seg,
               cb.between(
                   po.get(IUV_FIELD),
                   segregationCode,
                   CommonUtil.getSegregationCodeEnd(segregationCode)));
     }
     predicates.add(seg);
   }

   sq.select(cb.literal(1)).where(cb.and(predicates.toArray(new Predicate[0])));

   return cb.exists(sq);
 }
}