package it.gov.pagopa.debtposition.repository.apd.specification;

import it.gov.pagopa.debtposition.entity.apd.PaymentPosition;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByOrganizationFiscalCode implements Specification<PaymentPosition> {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -2618936236986229586L;

  private String organizationFiscalCode;

  @Override
  public Predicate toPredicate(
      Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
    query.distinct(true);
    if (organizationFiscalCode == null) {
      return cb.isTrue(cb.literal(true)); // always true = no filtering
    }
    return cb.equal(root.get("organizationFiscalCode"), this.organizationFiscalCode);
  }
}
