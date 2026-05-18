package it.gov.pagopa.debtposition.service;

import java.time.LocalDateTime;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;

@Service
public class DebtPositionStatusBatchService {

  private final PaymentPositionRepository paymentPositionRepository;

  public DebtPositionStatusBatchService(
      PaymentPositionRepository paymentPositionRepository) {
    this.paymentPositionRepository = paymentPositionRepository;
  }

  @Transactional(
      propagation = Propagation.REQUIRES_NEW,
      timeout = 30)
  public int updatePublishedToValidBatch(
      LocalDateTime currentDate,
      int batchSize) {

    return paymentPositionRepository.updatePaymentPositionStatusToValidBatch(
        currentDate,
        DebtPositionStatus.VALID.name(),
        batchSize);
  }

  @Transactional(
      propagation = Propagation.REQUIRES_NEW,
      timeout = 30)
  public int updateValidToExpiredBatch(
      LocalDateTime currentDate,
      int batchSize) {

    return paymentPositionRepository.updatePaymentPositionStatusToExpiredBatch(
        currentDate,
        batchSize);
  }
}