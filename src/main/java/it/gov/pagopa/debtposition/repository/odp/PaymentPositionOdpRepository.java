package it.gov.pagopa.debtposition.repository.odp;

import it.gov.pagopa.debtposition.entity.odp.PaymentPositionOdp;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PaymentPositionOdpRepository
        extends JpaRepository<PaymentPositionOdp, Long>,
        JpaSpecificationExecutor<PaymentPositionOdp>,
        PagingAndSortingRepository<PaymentPositionOdp, Long> {

    // Derived Query - using method naming convention - get PaymentPositionOdp by iupd
    PaymentPositionOdp findByIupd(String iupd);
    List<PaymentPositionOdp> findAllByIupdIn(List<String> iupds);
}
