package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.PaymentOption;

import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * @author aacitelli
 */

@Repository
public interface PaymentOptionRepository extends JpaRepository<PaymentOption, Long>, JpaSpecificationExecutor<PaymentOption> {
    // Derived Query - using method naming convention
    //Optional<PaymentOption> findByOrganizationFiscalCodeAndNav(String organizationFiscalCode, String nav);  // search only by nav
	Optional<PaymentOption> findByOrganizationFiscalCodeAndIuv(String organizationFiscalCode, String iuv);  // search only by iuv
	
	
	// TODO #naviuv: temporary regression management: search by nav or iuv
	Optional<PaymentOption> findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(String organizationFiscalCodeIuv, String iuv, String organizationFiscalCodeNav, String nav);

	// Derived Query - using method naming convention - get all PaymentOption by payment_position_id and in the specified statuses
	List<PaymentOption> findByPaymentPositionIdAndStatusIn(Long paymentPositionId, List<PaymentOptionStatus> statusList);
}

