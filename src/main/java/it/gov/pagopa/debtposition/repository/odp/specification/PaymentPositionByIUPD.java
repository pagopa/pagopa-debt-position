package it.gov.pagopa.debtposition.repository.odp.specification;

import it.gov.pagopa.debtposition.entity.odp.PaymentPosition;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByIUPD implements Specification<PaymentPosition> {

  /** generated serialVersionUID */
  private static final long serialVersionUID = 5225764424085040384L;

  private String debtPositionNumber;

  @Override
  public Predicate toPredicate(
      Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
    if (debtPositionNumber == null) {
      return cb.isTrue(cb.literal(true)); // always true = no filtering
    }
    return cb.equal(root.get("iupd"), this.debtPositionNumber);
  }
}
