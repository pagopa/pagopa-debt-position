package it.gov.pagopa.debtposition.repository.specification;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.*;

@NoArgsConstructor
@AllArgsConstructor
public class PaymentPositionBySegregationCode implements Specification<PaymentPosition> {
    private static final String IUV_FIELD = "iuv";
    private static final String PAYMENT_OPT_JOIN = "paymentOption";

    private String segregationCode;
    private String segregationCodeEnd;

    public PaymentPositionBySegregationCode(String segregationCode) {
        this.segregationCode = segregationCode;

        int length = segregationCode.length() - 1;
        int nextChar = segregationCode.toCharArray()[length] + 1;
        this.segregationCodeEnd = segregationCode.substring(0, length) + (char) nextChar;
    }

    public Predicate toPredicate(Root<PaymentPosition> root, CriteriaQuery<?> query, CriteriaBuilder cb) {
        Join<?, ?> ppOptionsJoin = root.join(PAYMENT_OPT_JOIN, JoinType.INNER);

        if (segregationCode != null) {
            return cb.between(ppOptionsJoin.get(IUV_FIELD), segregationCode, segregationCodeEnd);
        }

        return cb.isTrue(cb.literal(true));
    }
}
