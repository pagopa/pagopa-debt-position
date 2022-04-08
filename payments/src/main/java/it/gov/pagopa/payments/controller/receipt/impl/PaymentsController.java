package it.gov.pagopa.payments.controller.receipt.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;
import javax.validation.constraints.Positive;

import it.gov.pagopa.payments.model.ReceiptModelResponse;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import com.microsoft.azure.storage.ResultSegment;

import it.gov.pagopa.payments.controller.receipt.IPaymentsController;
import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.model.ReceiptsInfo;
import it.gov.pagopa.payments.service.PaymentsService;
import it.gov.pagopa.payments.utils.CommonUtil;
import lombok.extern.slf4j.Slf4j;

@Controller
@Slf4j
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
	public ResponseEntity<ReceiptsInfo> getOrganizationReceipts(String organizationFiscalCode, @Positive Integer limit,
			@Positive Integer page, @Valid String debtor) {
		log.info(String.format(LOG_BASE_HEADER_INFO, "GET", "getOrganizationReceipts",
				String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode) + "; debtor= " + debtor));
		
		ResultSegment<ReceiptEntity> receipts = paymentsService.getOrganizationReceipts(limit,page,organizationFiscalCode,debtor);
		return new ResponseEntity<>(ReceiptsInfo.builder()
                .receiptsList(receipts.getResults().stream().map(receiptEntity -> modelMapper.map(receiptEntity, ReceiptModelResponse.class)).collect(Collectors.toList()))
                .pageInfo(CommonUtil.buildPageInfo(receipts))
                .build(),
                HttpStatus.OK);
	}

}
