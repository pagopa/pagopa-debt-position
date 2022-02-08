package it.gov.pagopa.debtposition.controller.pd.actions.api.impl;

import javax.validation.constraints.NotBlank;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import it.gov.pagopa.debtposition.controller.pd.actions.api.IDebtPositionActionsController;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.service.pd.actions.PaymentPositionActionsService;
import it.gov.pagopa.debtposition.util.HttpStatusExplainMessage;
import lombok.extern.slf4j.Slf4j;


@Controller
@Slf4j
public class DebtPositionActionsController implements IDebtPositionActionsController {
	
	@Autowired
	private PaymentPositionActionsService paymentPositionActionsService;
	
	private static final String LOG_BASE_HEADER_INFO   = "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
	private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; iupd= %s";

	@Override
	public ResponseEntity<String> publishDebtPosition(@NotBlank String organizationFiscalCode,@NotBlank String iupd) {
		log.info(String.format(LOG_BASE_HEADER_INFO,"POST","publishDebtPosition", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iupd)));
		
		PaymentPosition publishedDebtPos = paymentPositionActionsService.publish(organizationFiscalCode, iupd);
		if (null != publishedDebtPos) {
			return new ResponseEntity<>(HttpStatusExplainMessage.DEBT_POSITION_PUBLISHED, HttpStatus.OK);
		}
		
		throw new AppException(AppError.DEBT_POSITION_PUBLISH_FAILED, organizationFiscalCode);
	}

}
