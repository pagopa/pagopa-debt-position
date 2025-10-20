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
        value = """
            UPDATE transfer AS tr
            SET iban = :newIban,
                last_updated_date = :currentDate
            WHERE tr.id IN (
                SELECT tr2.id
                FROM transfer tr2
                JOIN payment_option po ON tr2.payment_option_id = po.id
                JOIN payment_position pp ON po.payment_position_id = pp.id
                WHERE tr2.iban = :oldIban
                  AND po.status IN (:poStatus)
                  AND pp.organization_fiscal_code = :organizationFiscalCode
                  AND pp.status IN (:ppStatus)
                LIMIT :limit
            )
            """
    )
    int updateTransferIban(
        @Param("organizationFiscalCode") String organizationFiscalCode,
        @Param("oldIban") String oldIban,
        @Param("newIban") String newIban,
        @Param("currentDate") LocalDateTime currentDate,
        @Param("poStatus") List<String> poStatus,
        @Param("ppStatus") List<String> ppStatus,
        @Param("limit") int limit
    );
}
