package it.gov.pagopa.debtposition.service.payments;

import static it.gov.pagopa.debtposition.service.payments.PaymentsService.updateAmountsWithNotificationFee;

import java.time.LocalDateTime;
import java.time.ZoneOffset;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.service.payments.PaymentsService.PaymentOptionNotificationFeeContext;

@Service
public class NotificationFeeUpdateService {

  private final PaymentOptionRepository paymentOptionRepository;

  public NotificationFeeUpdateService(
      PaymentOptionRepository paymentOptionRepository) {
    this.paymentOptionRepository = paymentOptionRepository;
  }

  @Transactional(readOnly = true)
  public PaymentOptionNotificationFeeContext loadContext(
      String organizationFiscalCode,
      String nav) {

    PaymentOption paymentOption =
        paymentOptionRepository
            .findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
                organizationFiscalCode,
                nav,
                organizationFiscalCode,
                nav)
            .orElseThrow(
                () -> new AppException(
                    AppError.PAYMENT_OPTION_NOT_FOUND,
                    organizationFiscalCode,
                    nav));

    if (!PaymentOptionStatus.PO_UNPAID.equals(paymentOption.getStatus())) {
      throw new AppException(
          AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_NOT_UPDATABLE,
          organizationFiscalCode,
          nav);
    }

    return new PaymentOptionNotificationFeeContext(
        paymentOption.getId(),
        paymentOption.getOrganizationFiscalCode(),
        paymentOption.getNav());
  }

  @Transactional
  public PaymentOption applyNotificationFeeUpdate(
      Long paymentOptionId,
      Long notificationFeeAmount,
      Boolean paymentInProgress) {

    PaymentOption paymentOption =
        paymentOptionRepository.findById(paymentOptionId)
            .orElseThrow(
                () -> new AppException(
                    AppError.PAYMENT_OPTION_NOT_FOUND,
                    String.valueOf(paymentOptionId),
                    ""));

    updateAmountsWithNotificationFee(
        paymentOption,
        paymentOption.getOrganizationFiscalCode(),
        notificationFeeAmount);

    paymentOption.setPaymentInProgress(paymentInProgress);
    paymentOption.setLastUpdatedDate(LocalDateTime.now(ZoneOffset.UTC));
    paymentOption.setLastUpdatedDateNotificationFee(LocalDateTime.now(ZoneOffset.UTC));

    return paymentOptionRepository.saveAndFlush(paymentOption);
  }
  
  @Transactional
  public PaymentOption applyNotificationFeeUpdate(
      Long paymentOptionId,
      Long notificationFeeAmount) {

    PaymentOption paymentOption =
        paymentOptionRepository.findById(paymentOptionId)
            .orElseThrow(
                () -> new AppException(
                    AppError.PAYMENT_OPTION_NOT_FOUND,
                    String.valueOf(paymentOptionId),
                    ""));

    updateAmountsWithNotificationFee(
        paymentOption,
        paymentOption.getOrganizationFiscalCode(),
        notificationFeeAmount);

    paymentOption.setLastUpdatedDate(LocalDateTime.now(ZoneOffset.UTC));
    paymentOption.setLastUpdatedDateNotificationFee(LocalDateTime.now(ZoneOffset.UTC));

    return paymentOptionRepository.saveAndFlush(paymentOption);
  }
}