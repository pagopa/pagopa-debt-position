package it.gov.pagopa.debtposition.repository.apd.specification;

import it.gov.pagopa.debtposition.entity.apd.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByStatus implements Specification<PaymentPosition> {

  private DebtPositionStatus status;

  @Override
  public Predicate toPredicate(
      Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {

    if (status == null) {
      return cb.isTrue(cb.literal(true)); // always true = no filtering
    }
    return cb.equal(root.get("status"), this.status);
  }
}
