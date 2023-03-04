package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDate;
import java.time.LocalTime;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByPaymentDate implements Specification<PaymentPosition> {

    private LocalDate paymentDate;

    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
        if (paymentDate == null) {
            return cb.isTrue(cb.literal(true)); // always true = no filtering
        }

        return cb.between(root.get("paymentDate"), paymentDate.atStartOfDay(), paymentDate.atTime(LocalTime.MAX));
    }
}
