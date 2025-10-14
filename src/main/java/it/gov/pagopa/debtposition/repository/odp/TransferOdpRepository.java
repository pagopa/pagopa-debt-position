package it.gov.pagopa.debtposition.repository.odp;

import it.gov.pagopa.debtposition.entity.odp.TransferOdp;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface TransferOdpRepository extends JpaRepository<TransferOdp, Long> {

    @Modifying
    @Transactional
    @Query(
            nativeQuery = true,
            value = "UPDATE odp.transfer as tr " +
                    "SET iban = :newIban, last_updated_date = :currentDate " +
                    "where id in (" +
                    "SELECT tr.id " +
                    "from odp.transfer tr join odp.installment AS inst ON tr.installment_id = inst.id " +
                    "JOIN odp.payment_position AS pp ON inst.payment_position_id = pp.id " +
                    "WHERE tr.iban=:oldIban and inst.status in (:instStatus) and " +
                    "pp.organization_fiscal_code = :organizationFiscalCode and pp.status in (:ppStatus) " +
                    "limit :limit)")
    int updateTransferIban(
            @Param(value = "organizationFiscalCode") String organizationFiscalCode,
            @Param(value = "oldIban") String oldIban,
            @Param(value = "newIban") String newIban,
            @Param(value = "currentDate") LocalDateTime currentDate,
            @Param(value = "instStatus") List<String> instStatus,
            @Param(value = "ppStatus") List<String> ppStatus,
            @Param(value = "limit") int limit
    );
}
