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
        // testo solo la dateFrom, la dateTo sarà sicuramente not null arrivati a questa if
        else if (dateFrom != null) {
            return cb.between(root.get(DATE_FIELD), dateFrom, dateTo);
        }

        return cb.isTrue(cb.literal(true));
    }
}
