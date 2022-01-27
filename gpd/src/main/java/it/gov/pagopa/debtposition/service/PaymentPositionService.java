package it.gov.pagopa.debtposition.service;

import java.util.Optional;

import javax.validation.constraints.Positive;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
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
		
		Example<PaymentPosition> filters = CommonUtil.getFilters(PaymentPosition.builder()
                .organizationFiscalCode(filterAndOrder.getFilter().getOrganizationFiscalCode())
                .build());
		
		// se è valorizzata solo la due_date_from
		if (null != filterAndOrder.getFilter().getDueDateFrom() && null == filterAndOrder.getFilter().getDueDateTo()) {
			return paymentPositionRepository.findByPaymentOptionDueDateGreaterThanEqual(filterAndOrder.getFilter().getDueDateFrom(), pageable);
		}
		// se è valorizzata solo la due_date_to
		else if (null == filterAndOrder.getFilter().getDueDateFrom() && null != filterAndOrder.getFilter().getDueDateTo()) {
			return paymentPositionRepository.findByPaymentOptionDueDateLessThanEqual(filterAndOrder.getFilter().getDueDateFrom(), pageable);
		}
		// se sono valorizzate entrambe
		else if (null != filterAndOrder.getFilter().getDueDateFrom() && null != filterAndOrder.getFilter().getDueDateTo()) {
			return paymentPositionRepository.findByPaymentOptionDueDateBetween(filterAndOrder.getFilter().getDueDateFrom(), pageable);
		} 
		else {
			return paymentPositionRepository.findAll(filters, pageable);
		}
		
	}
}
