package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.*;
import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByPaymentDate implements Specification<PaymentPosition> {

    private LocalDateTime dateFrom;
    private LocalDateTime dateTo;

    private static final String DATE_FIELD = "paymentDate";

    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {

        if (dateFrom != null && dateTo == null) {
            return cb.greaterThanOrEqualTo(root.get(DATE_FIELD), dateFrom);
        } else if (dateFrom == null && dateTo != null) {
            return cb.lessThanOrEqualTo(root.get(DATE_FIELD), dateTo);
        }
        // The execution proceeds on this branch in only 2 cases: dateFrom and dateTo equal null or both different from null,
        // to check the last case just apply the condition on one of the two dates
        else if (dateFrom != null) {
            return cb.between(root.get(DATE_FIELD), dateFrom, dateTo);
        }

        return cb.isTrue(cb.literal(true));
    }
}
