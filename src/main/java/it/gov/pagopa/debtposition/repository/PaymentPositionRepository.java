package it.gov.pagopa.debtposition.repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;

/**
 * @author aacitelli
 */

@Repository
public interface PaymentPositionRepository extends JpaRepository<PaymentPosition, Long>,
        JpaSpecificationExecutor<PaymentPosition>, PagingAndSortingRepository<PaymentPosition, Long> {

    @Modifying
    @Query("update PaymentPosition pp set pp.status = :status, pp.lastUpdatedDate = :currentDate, pp.version=pp.version+1 where pp.validityDate IS NOT NULL and pp.validityDate <= :currentDate and pp.status='PUBLISHED'")
    int updatePaymentPositionStatusToValid(@Param(value = "currentDate") LocalDateTime currentDate, @Param(value = "status") DebtPositionStatus status);

    // Regola 6 - Una posizione va in expired nel momento in cui si raggiunge la max_due_date, il flag switch_to_expired è impostato a TRUE e lo stato è a valid
    @Modifying
    @Query("update PaymentPosition pp set pp.status = :status, pp.lastUpdatedDate = :currentDate, pp.version=pp.version+1 where pp.maxDueDate < :currentDate and pp.status='VALID' and pp.switchToExpired IS TRUE")
    int updatePaymentPositionStatusToExpired(@Param(value = "currentDate") LocalDateTime currentDate, @Param(value = "status") DebtPositionStatus status);

    // Derived Query - using method naming convention - get parent PaymentPosition from child PaymentOption properties
    Optional<PaymentPosition> findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuv(String organizationFiscalCode, String iuv);

    // Derived Query - using method naming convention - get parent PaymentPosition from child PaymentOption and Transfer properties
    Optional<PaymentPosition> findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(String organizationFiscalCode, String iuv, String idTransfer);

    @Query("SELECT DISTINCT new it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean(pp.organizationFiscalCode as organizationFiscalCode) "
            + "FROM PaymentPosition pp WHERE pp.insertedDate >= :fromDate")
    List<OrganizationModelQueryBean> findDistinctOrganizationsByInsertedDate(LocalDateTime fromDate);
    
    @EntityGraph(attributePaths = "paymentOption")
    Page<PaymentPosition> findAll(Specification<PaymentPosition> spec, Pageable pageable);

}

