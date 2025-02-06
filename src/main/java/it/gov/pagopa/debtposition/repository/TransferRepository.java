package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.Transfer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface TransferRepository extends JpaRepository<Transfer, Long> {

    @Modifying
    @Transactional
    @Query(
            nativeQuery = true,
            value = "UPDATE apd.transfer as tr " +
                    "SET iban = :newIban, last_updated_date = :currentDate " +
                    "where id in (" +
                    "SELECT tr.id " +
                    "from apd.transfer tr join apd.payment_option AS po ON tr.payment_option_id = po.id " +
                    "JOIN apd.payment_position AS pp ON po.payment_position_id = pp.id " +
                    "WHERE tr.iban=:oldIban and po.status in (:poStatus) and " +
                    "pp.organization_fiscal_code = :organizationFiscalCode and pp.status in (:ppStatus) " +
                    "limit :limit)")
    int updateTransferIban(
            @Param(value = "organizationFiscalCode") String organizationFiscalCode,
            @Param(value = "oldIban") String oldIban,
            @Param(value = "newIban") String newIban,
            @Param(value = "currentDate") LocalDateTime currentDate,
            @Param(value = "poStatus") List<String> poStatus,
            @Param(value = "ppStatus") List<String> ppStatus,
            @Param(value = "limit") int limit
    );
}
