package it.gov.pagopa.debtposition.repository.odp;

import it.gov.pagopa.debtposition.entity.odp.TransferOdp;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TransferOdpRepository extends JpaRepository<TransferOdp, Long> {
}
