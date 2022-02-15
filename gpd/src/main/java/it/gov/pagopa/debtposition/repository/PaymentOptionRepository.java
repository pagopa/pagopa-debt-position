package it.gov.pagopa.debtposition.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import it.gov.pagopa.debtposition.entity.PaymentOption;

/**
 * @author aacitelli
 *
 */

@Repository
public interface PaymentOptionRepository extends JpaRepository<PaymentOption, Long>{
	// Derived Query - using method naming convention
	Optional<PaymentOption> findByOrganizationFiscalCodeAndIuv(String organizationFiscalCode, String iuv);
}

