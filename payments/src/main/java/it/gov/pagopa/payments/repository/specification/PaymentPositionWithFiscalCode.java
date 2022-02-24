package it.gov.pagopa.payments.repository.specification;

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
public class PaymentPositionWithFiscalCode implements Specification<PaymentPosition> {

    private static final long serialVersionUID = 4100418988221955504L;

    private String fiscalCode;

    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
	if (fiscalCode == null) {
	    return cb.isTrue(cb.literal(true)); // always true = no filtering
	}
	return cb.equal(root.get("organizationFiscalCode"), this.fiscalCode);
    }

}
