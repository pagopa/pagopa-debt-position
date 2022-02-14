package it.gov.pagopa.debtposition.service.payments;

import java.util.Optional;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;


@Service
public class PaymentsService {

	
	@Autowired
	private PaymentOptionRepository paymentOptionRepository;
	
	@Autowired
	private PaymentPositionRepository paymentPositionRepository;
	
	public PaymentOption getPaymentOptionByIUV (@NotBlank String organizationFiscalCode,
			@NotBlank String iuv) {

		Optional<PaymentOption> po = paymentOptionRepository.findByOrganizationFiscalCodeAndIuv(organizationFiscalCode, iuv);
		
		if (po.isEmpty()) {
			throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, iuv);
		}

		return po.get();
	}
	
	public PaymentOption pay (@NotBlank String organizationFiscalCode,
			@NotBlank String iuv, @NotNull @Valid PaymentOptionModel paymentOptionModel) {
		Optional<PaymentPosition> ppToPay = paymentPositionRepository.findByPaymentPositionOrganizationFiscalCodeAndIuv(organizationFiscalCode, iuv);
		
		if (ppToPay.isEmpty()) {
			throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, iuv);
		}

		//return ppToPay.get();
		return null;
	}
	

}
