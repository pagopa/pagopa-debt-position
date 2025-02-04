package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.Transfer;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface TransferRepository
    extends JpaRepository<Transfer, Long>, JpaSpecificationExecutor<Transfer> {

  @Modifying
  @Query(
      "update Transfer tr set tr.iban = :newIban, tr.lastUpdatedDate = :currentDate where"
          + " tr.paymentOption in (:paymentOptionList) and tr.iban=:oldIban")
  int updateTransferIban(
      @Param(value = "paymentOptionList") List<PaymentOption> paymentOptionList,
      @Param(value = "oldIban") String oldIban,
      @Param(value = "newIban") String newIban,
      @Param(value = "currentDate") LocalDateTime currentDate);
}
