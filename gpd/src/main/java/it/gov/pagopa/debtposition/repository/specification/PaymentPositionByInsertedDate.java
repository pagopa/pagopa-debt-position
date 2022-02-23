package it.gov.pagopa.debtposition.repository.specification;

import java.time.LocalDateTime;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.data.jpa.domain.Specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByInsertedDate implements Specification<PaymentPosition> {

	/**
     * generated serialVersionUID
     */
	private static final long serialVersionUID = -2429583946637003928L;
	
    private LocalDateTime since;
    
    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
    	if (since == null) {
    		return cb.isTrue(cb.literal(true)); // always true = no filtering
    	}
    	return cb.greaterThanOrEqualTo(root.get("insertedDate"), this.since);
    }
}
