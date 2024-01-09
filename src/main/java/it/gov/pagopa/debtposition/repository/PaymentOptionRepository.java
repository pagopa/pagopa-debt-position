package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * @author aacitelli
 */

@Repository
public interface PaymentOptionRepository extends JpaRepository<PaymentOption, Long>, JpaSpecificationExecutor<PaymentOption> {
    // Derived Query - using method naming convention
    Optional<PaymentOption> findByOrganizationFiscalCodeAndIuv(String organizationFiscalCode, String iuv);
}

