package it.gov.pagopa.debtposition.repository.odp;

import it.gov.pagopa.debtposition.entity.odp.PaymentPositionOdp;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PaymentPositionOdpRepository
        extends JpaRepository<PaymentPositionOdp, Long>,
        JpaSpecificationExecutor<PaymentPositionOdp>,
        PagingAndSortingRepository<PaymentPositionOdp, Long> {
}
