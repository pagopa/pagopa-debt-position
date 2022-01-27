package it.gov.pagopa.debtposition.repository;

import java.time.LocalDateTime;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.stereotype.Repository;

import it.gov.pagopa.debtposition.entity.PaymentPosition;

/**
 * @author aacitelli
 *
 */

@Repository
public interface PaymentPositionRepository extends JpaRepository<PaymentPosition, Long>, 
JpaSpecificationExecutor<PaymentPosition>, PagingAndSortingRepository<PaymentPosition, Long>{
			
	public Page<PaymentPosition> findByPaymentOptionDueDateGreaterThanEqual(LocalDateTime dueDate, Pageable pageable);
	public Page<PaymentPosition> findByPaymentOptionDueDateLessThanEqual   (LocalDateTime dueDate, Pageable pageable);
	public Page<PaymentPosition> findByPaymentOptionDueDateBetween         (LocalDateTime dueDateFrom, LocalDateTime dueDateTo, Pageable pageable);

}

