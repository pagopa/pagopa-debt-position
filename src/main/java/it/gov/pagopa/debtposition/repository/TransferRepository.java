package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.Transfer;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface TransferRepository extends JpaRepository<Transfer, Long>,
        JpaSpecificationExecutor<Transfer>, PagingAndSortingRepository<Transfer, Long> {

    // Derived Query - using method naming convention - get all Transfer by paymentOptionId
    List<Transfer> findByPaymentOptionId(Long paymentOptionId);

    @Modifying
    @Query("update Transfer tr set tr.newIban = :newIban, tr.lastUpdatedDate = :currentDate where tr.payment_option_id=:paymentOptionId and tr.iban=:oldIban")
    int updateTransferIban(@Param(value="paymentOptionId") Long paymentOptionId, @Param(value = "oldIban") String oldIban, @Param(value = "newIban") String newIban, @Param(value = "currentDate") LocalDateTime currentDate);
}
