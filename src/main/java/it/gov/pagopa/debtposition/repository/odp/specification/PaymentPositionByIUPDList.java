package it.gov.pagopa.debtposition.repository.odp.specification;

import it.gov.pagopa.debtposition.entity.odp.PaymentPosition;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public class PaymentPositionByIUPDList implements Specification<PaymentPosition> {

  private final List<String> iupdList;

  public PaymentPositionByIUPDList(List<String> iupdList) {
    this.iupdList = iupdList;
  }

  @Override
  public Predicate toPredicate(
      Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
    if (iupdList != null && !iupdList.isEmpty()) {
      return root.get("iupd").in(iupdList);
    } else {
      // always-true predicate, means that no filtering would be applied
      return cb.and();
    }
  }
}
