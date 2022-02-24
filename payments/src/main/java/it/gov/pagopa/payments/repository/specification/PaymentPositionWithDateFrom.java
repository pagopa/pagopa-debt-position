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
public class PaymentPositionWithDateFrom implements Specification<PaymentPosition> {

    private static final long serialVersionUID = 5590392031960332293L;

    private LocalDateTime dateFrom;

    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
	if (dateFrom == null) {
	    return cb.isTrue(cb.literal(true)); // always true = no filtering
	}
	return cb.greaterThanOrEqualTo(root.get("insertDate"), this.dateFrom);
    }

}
