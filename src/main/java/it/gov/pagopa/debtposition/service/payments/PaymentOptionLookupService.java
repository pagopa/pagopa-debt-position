package it.gov.pagopa.debtposition.service.payments;

import static it.gov.pagopa.debtposition.service.common.ExpirationHandler.handlePaymentPositionExpirationLogic;
import static it.gov.pagopa.debtposition.service.common.PaymentConflictValidator.checkAlreadyPaidInstallments;
import static it.gov.pagopa.debtposition.service.common.ValidityHandler.handlePaymentPositionValidTransition;

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

    // FSM Logic: Update state (PaymentPosition status) based on current time
    handlePaymentPositionValidTransition(paymentOption.getPaymentPosition());
    handlePaymentPositionExpirationLogic(paymentOption.getPaymentPosition());

    checkAlreadyPaidInstallments(paymentOption, nav, paymentOptionRepository);

    return paymentOption;
  }
}