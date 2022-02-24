package it.gov.pagopa.payments.repository.specification;

import java.time.LocalDateTime;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import it.gov.pagopa.payments.entity.PaymentPosition;
import org.springframework.data.jpa.domain.Specification;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionWithDateTo implements Specification<PaymentPosition> {

    private static final long serialVersionUID = 1860688037575544683L;
    
    private LocalDateTime dateTo;

    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
	if (dateTo == null) {
	    return cb.isTrue(cb.literal(true)); // always true = no filtering
	}
	return cb.lessThanOrEqualTo(root.get("insertDate"), this.dateTo);
    }

}
