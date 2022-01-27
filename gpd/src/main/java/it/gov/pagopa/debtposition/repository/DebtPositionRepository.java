package it.gov.pagopa.debtposition.repository;


import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import it.gov.pagopa.debtposition.entity.Debtor;

/**
 * @author aacitelli
 *
 */

@Repository
public interface DebtPositionRepository extends JpaRepository<Debtor, Long>, JpaSpecificationExecutor<Debtor>{

    Debtor findByFiscalCode(String fiscalCode);
}

