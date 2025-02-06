package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.Transfer;
import java.time.LocalDateTime;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
public interface TransferRepository extends JpaRepository<Transfer, Long> {

  @Modifying
  @Transactional
  @Query(
          nativeQuery = true,
          value = "UPDATE apd.transfer as tr " +
                  "SET iban = :newIban, last_updated_date = :currentDate " +
                  "FROM apd.payment_option as po, apd.payment_position as pp " +
                  "WHERE tr.payment_option_id = po.id and po.payment_position_id = pp.id and " +
                  "tr.iban=:oldIban and po.status in (:poStatus) and " +
                  "pp.organization_fiscal_code = :organizationFiscalCode and pp.status in (:ppStatus)")
  int updateTransferIban(
          @Param(value = "organizationFiscalCode") String organizationFiscalCode,
          @Param(value = "oldIban") String oldIban,
          @Param(value = "newIban") String newIban,
          @Param(value = "currentDate") LocalDateTime currentDate,
          @Param(value = "poStatus")List<String> poStatus,
          @Param(value = "ppStatus")List<String> ppStatus
  );
}
