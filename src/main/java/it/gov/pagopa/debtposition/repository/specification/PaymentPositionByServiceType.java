package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByServiceType implements Specification<PaymentPosition> {

    private ServiceType serviceType;

    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {

        if(serviceType == null)
            return cb.notEqual(root.get("serviceType"), ServiceType.WISP); // If serviceType is null, return true for all except WISP

        return cb.equal(root.get("serviceType"), serviceType);
    }
}
