package it.gov.pagopa.debtposition.controller.payments.api.impl;

import java.time.LocalDate;
import java.util.List;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import it.gov.pagopa.debtposition.controller.payments.api.IPaymentsController;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.OrganizationListModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.OrganizationModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.TransferModelResponse;
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

	@Override
	public ResponseEntity<PaymentOptionModelResponse> payPaymentOption(String organizationFiscalCode, String iuv,
			@Valid PaymentOptionModel paymentOptionModel) {
		log.info(String.format(LOG_BASE_HEADER_INFO,"POST","updateDebtPosition", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iuv)));
		
		PaymentOption paidPaymentOption = paymentsService.pay(organizationFiscalCode, iuv, paymentOptionModel);
		
		
		if (null != paidPaymentOption) {
			return new ResponseEntity<>(ObjectMapperUtils.map(paidPaymentOption, PaymentOptionModelResponse.class), HttpStatus.OK);
		}
		
		throw new AppException(AppError.PAYMENT_OPTION_PAY_FAILED, organizationFiscalCode, iuv);
	}

	@Override
	public ResponseEntity<TransferModelResponse> reportTransfer(String organizationFiscalCode, String iuv,
			String transferId) {
		Transfer reportedTransfer = paymentsService.report(organizationFiscalCode, iuv, transferId);
		if (null != reportedTransfer) {
			return new ResponseEntity<>(ObjectMapperUtils.map(reportedTransfer, TransferModelResponse.class), HttpStatus.OK);
		}
		throw new AppException(AppError.TRANSFER_REPORTING_FAILED, organizationFiscalCode, iuv, transferId);
	}

	@Override
	public ResponseEntity<OrganizationListModelResponse> getOrganizations(@Valid LocalDate since) {
		List<OrganizationModelQueryBean> ppListToAdd = paymentsService.getOrganizationsToAdd(since);
		List<OrganizationModelQueryBean> ppListToDelete = paymentsService.getOrganizationsToDelete(since);
				
		// flip bean to model
		List<OrganizationModelResponse> ppToAddResponseList = ObjectMapperUtils.mapAll(ppListToAdd, OrganizationModelResponse.class);
		List<OrganizationModelResponse> ppToDeleteResponseList = ObjectMapperUtils.mapAll(ppListToDelete, OrganizationModelResponse.class);
		
		return new ResponseEntity<>(OrganizationListModelResponse.builder()
		.add(ppToAddResponseList)
		.delete(ppToDeleteResponseList)
		.build(), HttpStatus.OK);
	}

}
