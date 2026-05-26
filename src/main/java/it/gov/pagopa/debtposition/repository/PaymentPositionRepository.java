package it.gov.pagopa.debtposition.repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

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

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import jakarta.persistence.LockModeType;

/**
 * @author aacitelli
 */
@Repository
public interface PaymentPositionRepository
    extends JpaRepository<PaymentPosition, Long>,
        JpaSpecificationExecutor<PaymentPosition>,
        PagingAndSortingRepository<PaymentPosition, Long> {
  
  /**
   * Batch version of updatePaymentPositionStatusToValid for OPTIMIZATION:
   * Updates maximum batchSize records to prevent connection pool exhaustion.
   * This should be called in a loop until fewer records are affected than batchSize.
   */
  @Modifying(clearAutomatically = true, flushAutomatically = true)
  @Query(value = """
      WITH candidate AS (
          SELECT pp.id
          FROM apd.payment_position pp
          WHERE pp.status = 'PUBLISHED'
          AND pp.payment_date IS NULL
          AND validity_date IS NOT NULL
          AND pp.validity_date <= :currentDate
          FOR UPDATE SKIP LOCKED
          LIMIT :batchSize
      )
      UPDATE apd.payment_position pp
      SET status = :status,
          last_updated_date = :currentDate,
          version = pp.version + 1
      FROM candidate c
      WHERE pp.id = c.id
      """, nativeQuery = true)
  int updatePaymentPositionStatusToValidBatch(
      @Param("currentDate") LocalDateTime currentDate,
      @Param("status") String status,
      @Param("batchSize") int batchSize);
  
  /**
   * Rule 6- For a debt position to EXPIRED, the following conditions must be met:
   *  1. all POs must be PO_UNPAID
   *  2. all POs must have switch_to_expired = true
   *  3. all POs must be expired
   *
   * Batch version of updatePaymentPositionStatusToExpired for OPTIMIZATION:
   * Updates maximum batchSize records to prevent connection pool exhaustion.
   * This should be called in a loop until fewer records are affected than batchSize.
   */
  @Modifying(clearAutomatically = true, flushAutomatically = true)
  @Query(value = """
      WITH candidate AS (
          SELECT pp.id
          FROM apd.payment_position pp
          WHERE pp.status = 'VALID'
          AND pp.payment_date IS NULL
          AND pp.switch_to_expired IS TRUE
          AND pp.max_due_date < :currentDate
          AND NOT EXISTS (
              SELECT 1
              FROM apd.payment_option po
              WHERE po.payment_position_id = pp.id
              AND po.status <> 'PO_UNPAID'
          )
          FOR UPDATE SKIP LOCKED
          LIMIT :batchSize
      )
      UPDATE apd.payment_position pp
      SET status = 'EXPIRED',
          last_updated_date = :currentDate,
          version = pp.version + 1
      FROM candidate c
      WHERE pp.id = c.id
      """, nativeQuery = true)
  int updatePaymentPositionStatusToExpiredBatch(
      @Param("currentDate") LocalDateTime currentDate,
      @Param("batchSize") int batchSize);

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
