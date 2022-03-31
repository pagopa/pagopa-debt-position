package it.gov.pagopa.debtposition.repository;


import it.gov.pagopa.debtposition.entity.PaymentPosition;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

/**
 * @author aacitelli
 */

@Repository
public interface DebtPositionRepository extends JpaRepository<PaymentPosition, Long>, JpaSpecificationExecutor<PaymentPosition> {

}

