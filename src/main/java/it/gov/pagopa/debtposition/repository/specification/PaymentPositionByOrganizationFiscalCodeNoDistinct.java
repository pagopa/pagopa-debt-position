package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import org.springframework.data.jpa.domain.Specification;
import lombok.AllArgsConstructor;

@AllArgsConstructor
public class PaymentPositionByOrganizationFiscalCodeNoDistinct implements Specification<PaymentPosition> {
	
	private static final long serialVersionUID = -5267534063062183370L;
	
	private final String organizationFiscalCode;

	@Override
	public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
		if (organizationFiscalCode == null) {
			return cb.conjunction(); // CriteriaBuilder method to create a neutral predicate
		}
		
		return cb.equal(root.get("organizationFiscalCode"), organizationFiscalCode);
	}
}

