package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * @author aacitelli
 */

@Repository
public interface PaymentOptionRepository extends JpaRepository<PaymentOption, Long> {
    // Derived Query - using method naming convention
    Optional<PaymentOption> findByOrganizationFiscalCodeAndIuv(String organizationFiscalCode, String iuv);
}

