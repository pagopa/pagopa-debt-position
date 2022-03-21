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
import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class PaymentPositionActionsService {

	@Autowired
	private PaymentPositionCRUDService paymentPositionCRUDService;
	@Autowired
	private PaymentPositionRepository paymentPositionRepository;
	

	@Transactional
	public PaymentPosition publish(@NotBlank String organizationFiscalCode, @NotBlank String iupd) {
		PaymentPosition ppToPublish = paymentPositionCRUDService.getDebtPositionByIUPD(organizationFiscalCode, iupd);
		if (DebtPositionStatus.getPaymentPosNotPublishableStatus().contains(ppToPublish.getStatus())){
			throw new AppException(AppError.DEBT_POSITION_NOT_PUBLISHABLE, organizationFiscalCode, iupd);
		}
		
		LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
		ppToPublish.setPublishDate(currentDate);
		ppToPublish.setStatus(DebtPositionStatus.PUBLISHED);
		ppToPublish.setLastUpdatedDate(currentDate);
		// Regola 3 e Regola 4 - se non era stata prevista una data di inizio validità e la data di pubblicazione è < della min_due_date => sovrascrivo lo stato direttamente a VALID
		if (null == ppToPublish.getValidityDate() && ppToPublish.getMinDueDate().isAfter(currentDate)) {
			ppToPublish.setValidityDate(currentDate);
			ppToPublish.setStatus(DebtPositionStatus.VALID);
		}
		// Regola 5 - se la richiesta di pubblicazione è avvenuta dopo che una una delle opzioni di pagamento è scaduta (currentDate > min_due_date) viene rilanciato un errore
		else if (null == ppToPublish.getValidityDate() && ppToPublish.getMinDueDate().isBefore(currentDate)) {
			log.error ("Publish request occurred after the due date of a payment options has expired - "
					+ "[organizationFiscalCode= "+organizationFiscalCode+"; "
					+ "iupd= "+iupd+"; "
					+ "minDueDate= "+ppToPublish.getMinDueDate()+"; "
					+ "request publish date= "+currentDate
					+ "]");
			throw new AppException(AppError.DEBT_POSITION_PUBLISH_DUE_DATE_MISMATCH, organizationFiscalCode, iupd);
		}
		// Regola 2 - se era stata prevista una data di inizio validità e la richiesta di pubblicazione viene fatta successivamente a tale data viene rilanciato un errore
		else if (ppToPublish.getValidityDate().isBefore(currentDate)) {
			log.error ("Publish request occurred after the validity date has expired - "
					+ "[organizationFiscalCode= "+organizationFiscalCode+"; "
					+ "iupd= "+iupd+"; "
					+ "validityDate= "+ppToPublish.getValidityDate()+"; "
					+ "request publish date= "+currentDate
					+ "]");
			throw new AppException(AppError.DEBT_POSITION_PUBLISH_VALIDITY_DATE_MISMATCH, organizationFiscalCode, iupd);
		}
		
		return paymentPositionRepository.saveAndFlush(ppToPublish);
		
	}
	

	@Transactional
	public PaymentPosition invalidate(@NotBlank String organizationFiscalCode, @NotBlank String iupd) {
		PaymentPosition ppToInvalidate = paymentPositionCRUDService.getDebtPositionByIUPD(organizationFiscalCode, iupd);
		if (DebtPositionStatus.getPaymentPosNotIvalidableStatus().contains(ppToInvalidate.getStatus())){
			throw new AppException(AppError.DEBT_POSITION_NOT_INVALIDABLE, organizationFiscalCode, iupd);
		}
		LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
		ppToInvalidate.setStatus(DebtPositionStatus.INVALID);
		ppToInvalidate.setLastUpdatedDate(currentDate);
		return paymentPositionRepository.saveAndFlush(ppToInvalidate);
	}

}
