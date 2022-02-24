package it.gov.pagopa.payments.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

import it.gov.pagopa.payments.entity.PaymentOptions;

@Repository
public interface PaymentOptionsRepository
    extends JpaRepository<PaymentOptions, Long>, JpaSpecificationExecutor<PaymentOptions> {

  Optional<PaymentOptions> findByNotificationCode(String notificationCode);

  Optional<PaymentOptions> findByNotificationCodeAndFiscalCode(String notificationCode, String fiscalCode);
}
