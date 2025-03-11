package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

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
