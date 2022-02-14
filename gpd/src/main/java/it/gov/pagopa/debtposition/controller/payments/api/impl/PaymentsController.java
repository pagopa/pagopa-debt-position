package it.gov.pagopa.debtposition.controller.payments.api.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import it.gov.pagopa.debtposition.controller.payments.api.IPaymentsController;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.service.payments.PaymentsService;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import lombok.extern.slf4j.Slf4j;


@Controller
@Slf4j
public class PaymentsController implements IPaymentsController {
	
	
	@Autowired
	private PaymentsService paymentsService;
	
	
	private static final String LOG_BASE_HEADER_INFO   = "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
	private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; iupd= %s";
	
	@Override
	public ResponseEntity<PaymentOptionModelResponse> getOrganizationPaymentOptionByIUV(
			String organizationFiscalCode, String iuv) {
		log.info(String.format(LOG_BASE_HEADER_INFO,"GET","getOrganizationPaymentOptionByIUV", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iuv)));
		
		// flip entity to model
		PaymentOptionModelResponse paymentOptionResponse = ObjectMapperUtils.map(
	    		paymentsService.getPaymentOptionByIUV(organizationFiscalCode, iuv), 
				PaymentOptionModelResponse.class);
		
		return new ResponseEntity<>(paymentOptionResponse, HttpStatus.OK);
	}

}
