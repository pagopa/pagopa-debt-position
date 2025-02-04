package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

/**
 * @author aacitelli
 */
@Repository
public interface PaymentOptionRepository
    extends JpaRepository<PaymentOption, Long>, JpaSpecificationExecutor<PaymentOption> {
  // Derived Query - using method naming convention
  // Optional<PaymentOption> findByOrganizationFiscalCodeAndNav(String organizationFiscalCode,
  // String nav);  // search only by nav
  Optional<PaymentOption> findByOrganizationFiscalCodeAndIuv(
      String organizationFiscalCode, String iuv); // search only by iuv

  // TODO #naviuv: temporary regression management: search by nav or iuv
  Optional<PaymentOption> findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
      String organizationFiscalCodeIuv, String iuv, String organizationFiscalCodeNav, String nav);
}
