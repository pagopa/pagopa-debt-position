package it.gov.pagopa.debtposition.controller.pd.api.impl;

import java.time.LocalDate;
import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.Positive;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import it.gov.pagopa.debtposition.controller.pd.api.IDebtPositionController;
import it.gov.pagopa.debtposition.dto.DebtorDTO;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.entity.Debtor;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.filterandorder.Filter;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.filterandorder.Order.PaymentPositionOrder;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionsInfo;
import it.gov.pagopa.debtposition.service.DebtPositionService;
import it.gov.pagopa.debtposition.service.PaymentPositionService;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.HttpStatusExplainMessage;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;


@Controller
public class DebtPositionController implements IDebtPositionController {
	
	@Autowired
	private ModelMapper modelMapper;
	
	@Autowired
	private DebtPositionService debtPositionService;
	
	@Autowired
	private PaymentPositionService paymentPositionService;
	
	@Override
	public ResponseEntity<String> createDebtPosition(String organizationFiscalCode,
			@Valid DebtorDTO debtPositionDTO) {
		
		// convert DTO to entity
		Debtor debtPosition = modelMapper.map(debtPositionDTO, Debtor.class);
		
		Debtor createdDebtPos = debtPositionService.create(debtPosition, organizationFiscalCode);
		
		if (null != createdDebtPos) {
			return new ResponseEntity<>(HttpStatusExplainMessage.DEBT_POSITION_CREATED, HttpStatus.CREATED);
		}
		
		throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
	}

	@Override
	public ResponseEntity<PaymentPositionDTO> getOrganizationDebtPositionByIUPD(String organizationFiscalCode,
			String iupd) {
		
		
		// convert entity to DTO
		PaymentPositionDTO paymentPositionDTO = ObjectMapperUtils.map(
				paymentPositionService.getDebtPositionByIUPD(organizationFiscalCode, iupd), 
				PaymentPositionDTO.class);
		
		return new ResponseEntity<>(paymentPositionDTO, HttpStatus.OK);
	}

	@Override
	public ResponseEntity<PaymentPositionsInfo> getOrganizationDebtPositions(String organizationfiscalcode,
			@Positive Integer limit, @Positive Integer page, LocalDate dueDateFrom, LocalDate dueDateTo, 
			PaymentPositionOrder orderBy, Direction ordering) {
		
		// Create filter and order object
		FilterAndOrder filterOrder = FilterAndOrder.builder()
                .filter(Filter.builder()
                        .organizationFiscalCode(organizationfiscalcode)
                        .dueDateFrom(dueDateFrom != null ? dueDateFrom.atStartOfDay() : null)
                        .dueDateTo(dueDateTo != null ? dueDateTo.atStartOfDay() : null)
                        .build())
                .order(Order.builder()
                        .orderBy(orderBy)
                        .ordering(ordering)
                        .build())
                .build();
		
		
		Page<PaymentPosition> pagePP = paymentPositionService.getOrganizationDebtPositions(limit, page, filterOrder);
		
		// convert entity to DTO
		List<PaymentPositionDTO> ppDTOList = ObjectMapperUtils.mapAll(
				pagePP.toList(), 
				PaymentPositionDTO.class);
		
		return new ResponseEntity<>(PaymentPositionsInfo.builder()
        .ppList(ppDTOList)
        .pageInfo(CommonUtil.buildPageInfo(pagePP))
        .build(), 
        HttpStatus.OK);
		
		
	}

}
