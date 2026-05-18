package it.gov.pagopa.debtposition.service.payments;

import static it.gov.pagopa.debtposition.service.common.ExpirationHandler.handlePaymentPositionExpirationLogic;
import static it.gov.pagopa.debtposition.service.common.ValidityHandler.handlePaymentPositionValidTransition;
import static it.gov.pagopa.debtposition.service.common.PaymentConflictValidator.checkAlreadyPaidInstallmentsReadOnly;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import jakarta.validation.constraints.NotBlank;
import java.util.Optional;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class PaymentOptionLookupService {

  private final PaymentOptionRepository paymentOptionRepository;

  public PaymentOptionLookupService(PaymentOptionRepository paymentOptionRepository) {
    this.paymentOptionRepository = paymentOptionRepository;
  }

  // TODO #naviuv: temporary regression management --> the nav variable can also be evaluated with
  // iuv. Remove the comment when only nav managment is enabled
  @Transactional(readOnly = true)
  public PaymentOption getPaymentOptionByNAVInternal(
      @NotBlank String organizationFiscalCode, @NotBlank String nav) {

    Optional<PaymentOption> po =
        paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            organizationFiscalCode, nav, organizationFiscalCode, nav);

    if (po.isEmpty()) {
      throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
    }

    PaymentOption paymentOption = po.get();

   /* 
    * Read-only FSM evaluation logic: Update state (PaymentPosition status) based on current time.
    * These handlers may adjust the in-memory entity state used to build the response,
    * but this lookup must not persist changes or acquire write locks (for this reason the transaction is read-only).
    */
    handlePaymentPositionValidTransition(paymentOption.getPaymentPosition());
    handlePaymentPositionExpirationLogic(paymentOption.getPaymentPosition());
    
    // Apply the cross-payment visibility rules without acquiring database locks.
    checkAlreadyPaidInstallmentsReadOnly(paymentOption, nav);

    return paymentOption;
  }
}