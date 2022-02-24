package it.gov.pagopa.hubpa.payments.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;

@Repository
public interface PaymentPositionRepository
    extends JpaRepository<PaymentPosition, Long>, JpaSpecificationExecutor<PaymentPosition> {

  List<PaymentPosition> findAllByJobId(Long jobId);

  PaymentPosition findByIdAndStatus(Long jobId, Integer status);

  Optional<PaymentPosition> findById(String id);

}
