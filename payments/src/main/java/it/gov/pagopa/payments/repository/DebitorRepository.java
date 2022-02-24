package it.gov.pagopa.payments.repository;

import java.math.BigDecimal;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import it.gov.pagopa.payments.entity.Debitor;

@Repository
public interface DebitorRepository extends JpaRepository<Debitor, Long> {

    List<Debitor> findByPaymentPositionOrganizationFiscalCode(String fiscalCode);

    Debitor findByFiscalCode(String fiscalCode);

    long countByFiscalCodeAndPaymentPositionOrganizationFiscalCodeAndPaymentPositionAmount(String debitorFiscalCode,
	    String creditorFiscalCode, BigDecimal amount);
}
