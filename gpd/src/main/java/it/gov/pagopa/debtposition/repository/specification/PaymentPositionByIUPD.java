package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByIUPD implements Specification<PaymentPosition> {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = 5225764424085040384L;

    private String debtPositionNumber;

    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
        if (debtPositionNumber == null) {
            return cb.isTrue(cb.literal(true)); // always true = no filtering
        }
        return cb.equal(root.get("iupd"), this.debtPositionNumber);
    }

}
