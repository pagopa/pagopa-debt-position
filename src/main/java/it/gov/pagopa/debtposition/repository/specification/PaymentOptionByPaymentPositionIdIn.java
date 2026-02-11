package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import org.springframework.data.jpa.domain.Specification;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.List;

public class PaymentOptionByPaymentPositionIdIn implements Specification<PaymentOption> {


	private static final long serialVersionUID = -1350007136076964588L;
	private final List<Long> paymentPositionIds;

	public PaymentOptionByPaymentPositionIdIn(List<Long> paymentPositionIds) {
		this.paymentPositionIds = paymentPositionIds;
	}

	@Override
	public Predicate toPredicate(Root<PaymentOption> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
		// po.paymentPosition.id IN (:ids)
		return root.get("paymentPosition").get("id").in(paymentPositionIds);
	}
}
