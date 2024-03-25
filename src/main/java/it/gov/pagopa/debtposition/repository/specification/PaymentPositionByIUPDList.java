package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.util.List;

public class PaymentPositionByIUPDList implements Specification<PaymentPosition> {

    private final List<String> iupdList;

    public PaymentPositionByIUPDList(List<String> iupdList) {
        this.iupdList = iupdList;
    }

    @Override
    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
        if (iupdList != null && !iupdList.isEmpty()) {
            return root.get("iupd").in(iupdList);
        } else {
            // always-true predicate, means that no filtering would be applied
            return cb.and();
        }
    }
}
