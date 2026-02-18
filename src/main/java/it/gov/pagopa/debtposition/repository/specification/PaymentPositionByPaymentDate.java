package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import java.time.LocalDateTime;
import jakarta.persistence.criteria.*;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionByPaymentDate implements Specification<PaymentPosition> {

	private static final long serialVersionUID = -2050797176437259178L;
	private LocalDateTime dateFrom;
	private LocalDateTime dateTo;

	private static final String DATE_FIELD = "paymentDate";

	@Override
	public Predicate toPredicate(
			Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {

		Path<LocalDateTime> paymentDate = root.get(DATE_FIELD);

		if (dateFrom != null && dateTo != null) {
			return cb.between(paymentDate, dateFrom, dateTo);
		}
		if (dateFrom != null) {
			return cb.greaterThanOrEqualTo(paymentDate, dateFrom);
		}
		if (dateTo != null) {
			return cb.lessThanOrEqualTo(paymentDate, dateTo);
		}

		// no filter
		return cb.conjunction();
	}
}
