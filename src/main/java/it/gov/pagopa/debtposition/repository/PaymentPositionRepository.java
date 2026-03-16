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
  @Query("""
    update PaymentPosition pp
       set pp.status = :status,
           pp.lastUpdatedDate = :currentDate,
           pp.version = pp.version + 1
     where pp.status = 'PUBLISHED'
       and exists (
             select 1
               from PaymentOption po
              where po.paymentPosition = pp
                and po.validityDate is not null
                and po.validityDate <= :currentDate
       )
  """)
  int updatePaymentPositionStatusToValid(
      @Param("currentDate") LocalDateTime currentDate,
      @Param("status") DebtPositionStatus status);

  /**
   * Rule 6- For a debt position to EXPIRED, the following conditions must be met:
   *  1. all POs must be PO_UNPAID
   *  2. all POs must have switch_to_expired = true
   *  3. all POs must be expired
   */
	@Modifying
	@Query(value = """
			UPDATE apd.payment_position pp
			SET status = 'EXPIRED',
			last_updated_date = :currentDate,
			version = pp.version + 1
			WHERE pp.status = 'VALID'
			AND pp.payment_date IS NULL
			
			-- all POs must be PO_UNPAID
			AND NOT EXISTS (
			SELECT 1
			FROM apd.payment_option po1
			WHERE po1.payment_position_id = pp.id
			AND po1.status <> 'PO_UNPAID'
			)
			
			-- all POs must have switch_to_expired = true
			AND NOT EXISTS (
			SELECT 1
			FROM apd.payment_option po2
			WHERE po2.payment_position_id = pp.id
			AND po2.switch_to_expired = false
			)
			
			-- all POs must be expired
			AND NOT EXISTS (
			SELECT 1
			FROM apd.payment_option po3
			WHERE po3.payment_position_id = pp.id
			AND po3.due_date >= :currentDate
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
