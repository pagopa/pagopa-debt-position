package it.gov.pagopa.payments.controller.receipt.impl;

import java.util.stream.Collectors;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Positive;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.validation.annotation.Validated;

import it.gov.pagopa.payments.controller.receipt.IPaymentsController;
import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.model.PaymentsResult;
import it.gov.pagopa.payments.model.ReceiptModelResponse;
import it.gov.pagopa.payments.model.ReceiptsInfo;
import it.gov.pagopa.payments.service.PaymentsService;
import it.gov.pagopa.payments.utils.CommonUtil;
import lombok.extern.slf4j.Slf4j;

@Controller
@Slf4j
@Validated
public class PaymentsController implements IPaymentsController {

	private static final String LOG_BASE_HEADER_INFO = "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
	private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s";

 	@Autowired
 	private ModelMapper modelMapper;

	@Autowired 
	private PaymentsService paymentsService;

	@Override
	public ResponseEntity<String> getReceiptByIUV(String organizationFiscalCode, String iuv) {
		log.info(String.format(LOG_BASE_HEADER_INFO, "GET", "getReceiptByIUV",
				String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode) + "; iuv= " + iuv));
		ReceiptEntity receipt = paymentsService.getReceiptByOrganizationFCAndIUV(organizationFiscalCode, iuv);
		return new ResponseEntity<>(receipt.getDocument(), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<ReceiptsInfo> getOrganizationReceipts(String organizationFiscalCode,
			@Valid @Positive @Max(999) Integer limit, @Valid @Min(0) Integer page, String debtor, String service) {
		log.info(String.format(LOG_BASE_HEADER_INFO, "GET", "getOrganizationReceipts",
				String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode) + "; debtor= " + debtor + "; service= " + service));
		PaymentsResult<ReceiptEntity> receipts = paymentsService.getOrganizationReceipts(limit,page,organizationFiscalCode,debtor,service);
		return new ResponseEntity<>(ReceiptsInfo.builder()
                .receiptsList(receipts.getResults().stream().map(receiptEntity -> modelMapper.map(receiptEntity, ReceiptModelResponse.class)).collect(Collectors.toList()))
                .pageInfo(CommonUtil.buildPageInfo(receipts))
                .build(),
                HttpStatus.OK);
	}
	

	
	

}
