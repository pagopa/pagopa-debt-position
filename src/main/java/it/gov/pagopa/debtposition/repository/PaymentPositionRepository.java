package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * @author aacitelli
 */
@Repository
public interface PaymentPositionRepository
    extends JpaRepository<PaymentPosition, Long>,
        JpaSpecificationExecutor<PaymentPosition>,
        PagingAndSortingRepository<PaymentPosition, Long> {

  @Modifying
  @Query(
      "update PaymentPosition pp set pp.status = :status, pp.lastUpdatedDate = :currentDate,"
          + " pp.version=pp.version+1 where pp.validityDate IS NOT NULL and pp.validityDate <="
          + " :currentDate and pp.status='PUBLISHED'")
  int updatePaymentPositionStatusToValid(
      @Param(value = "currentDate") LocalDateTime currentDate,
      @Param(value = "status") DebtPositionStatus status);

  // Regola 6 - Una posizione va in expired nel momento in cui si raggiunge la max_due_date, il flag
  // switch_to_expired è impostato a TRUE e lo stato è a valid
  /*
  @Modifying
  @Query(
      "update PaymentPosition pp set pp.status = :status, pp.lastUpdatedDate = :currentDate,"
          + " pp.version=pp.version+1 where pp.maxDueDate < :currentDate and pp.status='VALID' and"
          + " pp.switchToExpired IS TRUE")
  int updatePaymentPositionStatusToExpired(
      @Param(value = "currentDate") LocalDateTime currentDate,
      @Param(value = "status") DebtPositionStatus status);*/
  
  /**
   * Native is used to avoid two-step recovery:
   * - SELECT PaymentPosition in join with installment
   * - UPDATE PaymentPosition pp SET ... WHERE pp.id IN :ids
   * verbose with two round-trips.
   */
  @Modifying
  @Query(value = """
      UPDATE payment_position pp
      SET status = 'EXPIRED',
          last_updated_date = :currentDate,
          version = pp.version + 1
      WHERE pp.max_due_date < :currentDate
        AND pp.status = 'VALID'
        AND pp.payment_date IS NULL
        AND EXISTS (
            SELECT 1
            FROM installment i
            WHERE i.payment_position_id = pp.id
              AND i.switch_to_expired = true
              AND i.status = 'PO_UNPAID'
        )
        AND NOT EXISTS (
            SELECT 1
            FROM installment i2
            WHERE i2.payment_position_id = pp.id
              AND i2.status <> 'PO_UNPAID'
        )
      """, nativeQuery = true)
  int updatePaymentPositionStatusToExpired(@Param("currentDate") LocalDateTime currentDate);

  // Derived Query - using method naming convention - get parent PaymentPosition from child
  // PaymentOption properties
  // Optional<PaymentPosition> findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionNav(String
  // organizationFiscalCode, String nav); // search only by nav

  // TODO #naviuv: temporary regression management: search by nav or iuv
  // see: https://www.baeldung.com/jpa-pessimistic-locking
  @Lock(LockModeType.PESSIMISTIC_WRITE)
  Optional<PaymentPosition>
      findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvOrPaymentOptionOrganizationFiscalCodeAndPaymentOptionNav(
          String organizationFiscalCodeIuv,
          String iuv,
          String organizationFiscalCodeNav,
          String nav);

  // Derived Query - using method naming convention - get parent PaymentPosition from child
  // PaymentOption and Transfer properties
  // see: https://www.baeldung.com/jpa-pessimistic-locking
  @Lock(LockModeType.PESSIMISTIC_WRITE)
  Optional<PaymentPosition>
      findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
          String organizationFiscalCode, String iuv, String idTransfer);

  @Query(
      "SELECT DISTINCT new"
          + " it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean(pp.organizationFiscalCode"
          + " as organizationFiscalCode) FROM PaymentPosition pp WHERE pp.insertedDate >="
          + " :fromDate")
  List<OrganizationModelQueryBean> findDistinctOrganizationsByInsertedDate(LocalDateTime fromDate);

  Page<PaymentPosition> findAll(Specification<PaymentPosition> spec, Pageable pageable);

  // Derived Query - using method naming convention - get all PaymentPosition by
  // organization_fiscal_code and in the specified statuses
  List<PaymentPosition> findByOrganizationFiscalCodeAndStatusIn(
      String organizationFiscalCode, List<DebtPositionStatus> statusList);
}
