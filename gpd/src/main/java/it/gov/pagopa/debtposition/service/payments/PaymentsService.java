package it.gov.pagopa.debtposition.service.payments;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;


@Service
public class PaymentsService {

	
	@Autowired
	private PaymentOptionRepository paymentOptionRepository;
	
	public PaymentOption getPaymentOptionByIUV (String organizationFiscalCode,
			String iuv) {

		Optional<PaymentOption> po = paymentOptionRepository.findByOrganizationFiscalCodeAndIuv(organizationFiscalCode, iuv);
		
		if (po.isEmpty()) {
			throw new AppException(AppError.DEBT_POSITION_NOT_FOUND, organizationFiscalCode, iuv);
		}

		return po.get();
	}

}
