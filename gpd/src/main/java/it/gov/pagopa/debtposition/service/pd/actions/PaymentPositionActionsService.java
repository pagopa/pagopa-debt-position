package it.gov.pagopa.debtposition.service.pd.actions;

import java.time.LocalDateTime;
import java.time.ZoneOffset;

import javax.validation.constraints.NotBlank;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.service.pd.crud.PaymentPositionCRUDService;


@Service
public class PaymentPositionActionsService {

	@Autowired
	private PaymentPositionCRUDService paymentPositionCRUDService;
	@Autowired
	private PaymentPositionRepository paymentPositionRepository;
	
	
	@Transactional(isolation = Isolation.SERIALIZABLE)
	public PaymentPosition publish(@NotBlank String organizationFiscalCode, @NotBlank String iupd) {
		PaymentPosition ppToPublish = paymentPositionCRUDService.getDebtPositionByIUPD(organizationFiscalCode, iupd);
		// TODO Probabilmente non ci sarà da verificare se esiste già un pagamento ma se la poszione sia in uno stato pubblicabile 
		if (DebtPositionStatus.getPaymentPosAlreadyPaidStatus().contains(ppToPublish.getStatus())){
			throw new AppException(AppError.DEBT_POSITION_PAYMENT_FOUND, organizationFiscalCode, iupd);
		}
		
		LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
		if (null == ppToPublish.getValidityDate()) {
			ppToPublish.setValidityDate(currentDate);
		}
		ppToPublish.setPublishDate(currentDate);
		ppToPublish.setStatus(DebtPositionStatus.PUBLISHED);
		
		return paymentPositionRepository.saveAndFlush(ppToPublish);
		
	}
	
	

}
