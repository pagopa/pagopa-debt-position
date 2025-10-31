package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import jakarta.persistence.LockModeType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

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

    // Regola 6 - Una posizione va in expired nel momento in cui si raggiunge la max_due_date, il flag
    // switch_to_expired è impostato a TRUE e lo stato è a valid

    /**
     * Native is used to avoid two-step recovery:
     * - SELECT PaymentPosition in join with PaymentOption
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
                  FROM payment_option po
                  WHERE po.payment_position_id = pp.id
                    AND po.switch_to_expired = true
                    AND EXISTS (
                        SELECT 1
                        FROM installment inst
                        WHERE inst.payment_option_id = po.id
                            AND inst.status = 'UNPAID'
                    )
              )
              AND NOT EXISTS (
                  SELECT 1
                  FROM installment inst2
                  WHERE inst2.payment_position_id = pp.id
                    AND inst2.status <> 'UNPAID'
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
    findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionInstallmentIuvOrPaymentOptionOrganizationFiscalCodeAndPaymentOptionInstallmentNav(
            String organizationFiscalCodeIuv,
            String iuv,
            String organizationFiscalCodeNav,
            String nav);

    // Derived Query - using method naming convention - get parent PaymentPosition from child
    // PaymentOption and Transfer properties
    // see: https://www.baeldung.com/jpa-pessimistic-locking
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    Optional<PaymentPosition>
    findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionInstallmentIuvAndPaymentOptionInstallmentTransferTransferId(
            String organizationFiscalCode, String iuv, String idTransfer);

    // Derived Query - using method naming convention - get parent PaymentPosition from child
    // PaymentOption and Installment properties
    Optional<PaymentPosition>
    findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionInstallmentIuv(
            String organizationFiscalCode, String iuv);

    @Query(
            "SELECT DISTINCT new"
                    + " it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean(pp.organizationFiscalCode"
                    + " as organizationFiscalCode) FROM PaymentPosition pp WHERE pp.insertedDate >="
                    + " :fromDate")
    List<OrganizationModelQueryBean> findDistinctOrganizationsByInsertedDate(LocalDateTime fromDate);

    Page<PaymentPosition> findAll(Specification<PaymentPosition> spec, Pageable pageable);
}
