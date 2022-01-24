package it.gov.pagopa.debtposition.service;

import java.util.List;

import org.hibernate.exception.ConstraintViolationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import it.gov.pagopa.debtposition.model.Debtor;
import it.gov.pagopa.debtposition.model.PaymentOption;
import it.gov.pagopa.debtposition.model.PaymentPosition;
import it.gov.pagopa.debtposition.repository.DebtPositionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByIUPD;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByOrganizationFiscalCode;
import it.gov.pagopa.debtposition.util.HttpStatusExplainMessage;

@Service
public class DebtPositionService {

	@Autowired
	private DebtPositionRepository debtPositionRepository;
	
	@Autowired
	private PaymentPositionRepository paymentPositionRepository;
	
	
	public Debtor create (Debtor debtPosition, String organizationFiscalCode, String iupd) {
		// Prima di creare la posizione debitoria bisogna controllare che IUPD e IUV, per l'EC, non siano già presenti
		
		
		try {
			/*
			Specification<PaymentPosition> spec = Specification.where(
					new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
					.and(new PaymentPositionByIUPD(iupd))
					);
			
			List<PaymentPosition> existingPaymentPositionList = paymentPositionRepository.findAll(spec);
			
			// Per lo stesso debitore:
			// Non deve esistere già una payment position con lo stesso organization_fiscal_code e IUPD.
			// Non deve esistere giè una payment option con lo organization_fiscal_code e IUV.
			List <PaymentPosition> existingPaymentPositionList = existingDebtor.getPaymentPosition();
			for (PaymentPosition existingPp: existingPaymentPositionList) {
				if (this.checkOrgCFAndIupd(debtPosition.getPaymentPosition(), 
						existingPp.getOrganizationFiscalCode(), 
						existingPp.getIupd())) {
					throw new ResponseStatusException(HttpStatus.CONFLICT, HttpStatusExplainMessage.DEBT_POSITION_ALREADY_EXIST);
				}
				if (this.checkOrgCFAndIuv(debtPosition, existingPp.getPaymentOption())) {
					throw new ResponseStatusException(HttpStatus.CONFLICT, HttpStatusExplainMessage.DEBT_POSITION_ALREADY_EXIST);
				}
			}*/
			// Inserisco la posizione debitoria
			return debtPositionRepository.saveAndFlush(debtPosition);
		} catch (ConstraintViolationException e) {
			throw new ResponseStatusException(HttpStatus.CONFLICT, HttpStatusExplainMessage.DEBT_POSITION_ALREADY_EXIST);
		}
	}
	
	private boolean checkOrgCFAndIupd(final List <PaymentPosition> list, final String organizationFiscalCode, final String iupd){
	   return list.stream()
			   .anyMatch(o -> o.getOrganizationFiscalCode().equals(organizationFiscalCode) && o.getIupd().equals(iupd));
	}
	
	private boolean checkOrgCFAndIuv(final Debtor debtPosition, final List <PaymentOption> existingPOList){		
		for (PaymentOption existingPo: existingPOList) {
			for (PaymentPosition pp: debtPosition.getPaymentPosition()) {
				if (this.containsOrgCFAndIuv(pp.getPaymentOption(), existingPo.getOrganizationFiscalCode(), existingPo.getIuv())) {
					return true;
				}
			}
		}
		return false;
	}
	
	private boolean containsOrgCFAndIuv(final List <PaymentOption> list, final String organizationFiscalCode, final String iuv){
		   return list.stream()
				   .anyMatch(o -> o.getOrganizationFiscalCode().equals(organizationFiscalCode) && o.getIuv().equals(iuv));
		}
	
}
