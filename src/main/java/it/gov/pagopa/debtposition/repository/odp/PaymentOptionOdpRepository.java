package it.gov.pagopa.debtposition.repository.odp;

import it.gov.pagopa.debtposition.entity.odp.PaymentOptionOdp;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

/**
 * @author aacitelli
 */
@Repository
public interface PaymentOptionOdpRepository
        extends JpaRepository<PaymentOptionOdp, Long>, JpaSpecificationExecutor<PaymentOptionOdp> {
    // Derived Query - using method naming convention - get PaymentOption by iuv
    PaymentOptionOdp findByIuv(String iuv);
}
