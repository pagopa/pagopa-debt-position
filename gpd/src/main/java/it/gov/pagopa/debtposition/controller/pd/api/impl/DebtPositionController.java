package it.gov.pagopa.debtposition.controller.pd.api.impl;

import java.util.List;

import javax.validation.Valid;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import it.gov.pagopa.debtposition.controller.pd.api.IDebtPositionController;
import it.gov.pagopa.debtposition.dto.DebtorDTO;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.entity.Debtor;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.service.DebtPositionService;
import it.gov.pagopa.debtposition.service.PaymentPositionService;
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
	public ResponseEntity<String> createDebtPosition(String organizationFiscalCode, String debtPositionNumber,
			@Valid DebtorDTO debtPositionDTO) {
		
		// convert DTO to entity
		Debtor debtPosition = modelMapper.map(debtPositionDTO, Debtor.class);
		
		Debtor createdDebtPos = debtPositionService.create(debtPosition, organizationFiscalCode, debtPositionNumber);
		
		if (null != createdDebtPos) {
			return new ResponseEntity<>(HttpStatusExplainMessage.DEBT_POSITION_CREATED, HttpStatus.CREATED);
		}
		
		throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode, debtPositionNumber);
	}

	@Override
	public ResponseEntity<List<PaymentPositionDTO>> getPositionByNoticeNumber(String organizationFiscalCode,
			String debtPositionNumber) {
		
		// convert entity to DTO
		List<PaymentPositionDTO> listOfPaymentPositionDTO = ObjectMapperUtils.mapAll(
				paymentPositionService.getPositionByNoticeNumber(organizationFiscalCode, debtPositionNumber), 
				PaymentPositionDTO.class);
		
		return new ResponseEntity<>(listOfPaymentPositionDTO, HttpStatus.OK);
	}

}
