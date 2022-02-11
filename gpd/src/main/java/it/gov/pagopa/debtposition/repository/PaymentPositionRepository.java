package it.gov.pagopa.debtposition.repository;

import java.time.LocalDateTime;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;

/**
 * @author aacitelli
 *
 */

@Repository
public interface PaymentPositionRepository extends JpaRepository<PaymentPosition, Long>, 
JpaSpecificationExecutor<PaymentPosition>, PagingAndSortingRepository<PaymentPosition, Long>{
	
	@Modifying
	@Query("update PaymentPosition pp set pp.status = :status, pp.lastUpdatedDate = :currentDate, pp.version=pp.version+1 where pp.validityDate IS NOT NULL and pp.validityDate <= :currentDate and pp.status='PUBLISHED'")
	void updatePaymentPositionStatusToValid(@Param(value = "currentDate") LocalDateTime currentDate, @Param(value = "status") DebtPositionStatus status);
	
	// Regola 6 - Una posizione va in expired nel momento in cui si raggiunge la max_due_date e lo stato sia rimasto fermo in valid
	@Modifying
	@Query("update PaymentPosition pp set pp.status = :status, pp.lastUpdatedDate = :currentDate, pp.version=pp.version+1 where pp.maxDueDate < :currentDate and pp.status='VALID'")
	void updatePaymentPositionStatusToExpired(@Param(value = "currentDate") LocalDateTime currentDate, @Param(value = "status") DebtPositionStatus status);

}

