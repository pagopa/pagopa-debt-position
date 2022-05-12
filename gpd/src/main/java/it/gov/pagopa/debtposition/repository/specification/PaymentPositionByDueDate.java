package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByDueDate implements Specification<PaymentPosition> {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = 6534338388239897792L;
    private static final String DUEDATE_FIELD = "dueDate";
    private static final String PAYMENT_OPT_JOIN = "paymentOption";

    private LocalDateTime dateFrom;
    private LocalDateTime dateTo;

    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {

    	Join<?, ?> ppOptionsJoin = root.join(PAYMENT_OPT_JOIN, JoinType.INNER);

        if (dateFrom != null && dateTo == null) {
            return cb.greaterThanOrEqualTo(ppOptionsJoin.get(DUEDATE_FIELD), dateFrom);
        } else if (dateFrom == null && dateTo != null) {
            return cb.lessThanOrEqualTo(ppOptionsJoin.get(DUEDATE_FIELD), dateTo);
        }
        // testo solo la dateFrom, la dateTo sarà sicuramente not null arrivati a questa if
        else if (dateFrom != null) {
            return cb.between(ppOptionsJoin.get(DUEDATE_FIELD), dateFrom, dateTo);
        }

        return cb.isTrue(cb.literal(true));
    }
}
