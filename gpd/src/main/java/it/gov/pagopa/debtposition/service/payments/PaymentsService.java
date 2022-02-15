package it.gov.pagopa.debtposition.service.payments;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Optional;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.validation.DebtPositionValidation;


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
	
	@Transactional(isolation = Isolation.SERIALIZABLE)
	public PaymentOption pay (@NotBlank String organizationFiscalCode,
			@NotBlank String iuv, @NotNull @Valid PaymentOptionModel paymentOptionModel) {

		
		Optional<PaymentPosition> ppToPay = paymentPositionRepository.findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuv(organizationFiscalCode, iuv);
		
		if (ppToPay.isEmpty()) {
			throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, iuv);
		}

		DebtPositionValidation.checkPaymentPositionPayability(ppToPay.get(), iuv);
		
		return this.updatePaymentStatus(ppToPay.get(), iuv, paymentOptionModel);
	}
	
	private PaymentOption updatePaymentStatus (PaymentPosition pp, String iuv, PaymentOptionModel paymentOptionModel) {
		
		LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
		PaymentOption poToPay = null;
		
		for (PaymentOption po : pp.getPaymentOption()) {
			if (po.getIuv().equals(iuv)) {
				po.setLastUpdatedDate(currentDate);
				po.setPaymentDate(paymentOptionModel.getPaymentDate());
				po.setPaymentMethod(paymentOptionModel.getPaymentMethod());
				po.setPspCompany(paymentOptionModel.getPspCompany());
				po.setIdReceipt(paymentOptionModel.getIdReceipt());
				po.setStatus(PaymentOptionStatus.PO_PAID);
				if (Boolean.TRUE.equals(po.getIsPartialPayment())) {
					pp.setStatus(DebtPositionStatus.PARTIALLY_PAID);
				}
				else {
					pp.setStatus(DebtPositionStatus.PAID);
				}
				
				poToPay = po;
			}
		}
		pp.setLastUpdatedDate(currentDate);
		// salvo l'aggiornamento del pagamento
		paymentPositionRepository.saveAndFlush(pp);
		return poToPay;
	}
	

}
