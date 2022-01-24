package it.gov.pagopa.debtposition.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import it.gov.pagopa.debtposition.model.PaymentPosition;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByIUPD;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByOrganizationFiscalCode;



@Service
public class PaymentPositionService {
	
	@Autowired
	private PaymentPositionRepository paymentPositionRepository;
	
	public List<PaymentPosition> getPositionByNoticeNumber (String organizationFiscalCode,
			String iupd) {
		
		Specification<PaymentPosition> spec = Specification.where(
				new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
				.and(new PaymentPositionByIUPD(iupd))
				);
		
		return paymentPositionRepository.findAll(spec);
		
	}
}
