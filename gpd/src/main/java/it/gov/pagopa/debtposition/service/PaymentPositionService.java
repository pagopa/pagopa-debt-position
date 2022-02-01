package it.gov.pagopa.debtposition.service;

import java.util.Optional;

import javax.validation.constraints.Positive;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByDueDate;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByIUPD;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByOrganizationFiscalCode;
import it.gov.pagopa.debtposition.util.CommonUtil;



@Service
public class PaymentPositionService {
	
	@Autowired
	private PaymentPositionRepository paymentPositionRepository;
	
	public PaymentPosition getDebtPositionByIUPD (String organizationFiscalCode,
			String iupd) {
		
		Specification<PaymentPosition> spec = Specification.where(
				new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
				.and(new PaymentPositionByIUPD(iupd))
				);
		
		Optional<PaymentPosition> pp = paymentPositionRepository.findOne(spec);
		if (pp.isEmpty()) {
			throw new AppException(AppError.DEBT_POSITION_NOT_FOUND, organizationFiscalCode, iupd);
		}
		
		return pp.get();
	}
	
	public Page<PaymentPosition> getOrganizationDebtPositions (@Positive Integer limit, @Positive Integer pageNum, FilterAndOrder filterAndOrder){
		
		Pageable pageable = PageRequest.of(pageNum, limit, CommonUtil.getSort(filterAndOrder));
		
		Specification<PaymentPosition> spec = Specification.where(
                new PaymentPositionByOrganizationFiscalCode(filterAndOrder.getFilter().getOrganizationFiscalCode())
                .and(new PaymentPositionByDueDate(
                        filterAndOrder.getFilter().getDueDateFrom(),
                        filterAndOrder.getFilter().getDueDateTo())));
			
		return paymentPositionRepository.findAll(spec, pageable);

	}
}
