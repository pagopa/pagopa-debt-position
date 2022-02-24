package it.gov.pagopa.hubpa.payments.repository.specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.data.jpa.domain.Specification;

import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionWithStatus implements Specification<PaymentPosition> {

    private static final long serialVersionUID = 4100418988221955504L;

    private Integer status;

    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
	if (status == null) {
	    return cb.isTrue(cb.literal(true)); // always true = no filtering
	}
	return cb.equal(root.get("status"), this.status);
    }

}
