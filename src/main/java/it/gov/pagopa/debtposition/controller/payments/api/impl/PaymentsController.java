package it.gov.pagopa.debtposition.controller.payments.api.impl;

import javax.validation.Valid;
import javax.validation.constraints.Pattern;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import it.gov.pagopa.debtposition.controller.payments.api.IPaymentsController;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.TransferModelResponse;
import it.gov.pagopa.debtposition.model.pd.NotificationFeeUpdateModel;
import it.gov.pagopa.debtposition.service.payments.PaymentsService;
import it.gov.pagopa.debtposition.util.CustomHttpStatus;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import lombok.extern.slf4j.Slf4j;


@Controller
@Slf4j
public class PaymentsController implements IPaymentsController {

    private static final String LOG_BASE_HEADER_INFO = "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
    private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; nav= %s";
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PaymentsService paymentsService;

    @Override
    public ResponseEntity<PaymentOptionWithDebtorInfoModelResponse> getOrganizationPaymentOptionByNAV(
            @Pattern(regexp = "\\d{1,30}") String organizationFiscalCode, @Pattern(regexp = "^\\d{1,30}$") String nav) {
        log.info(String.format(LOG_BASE_HEADER_INFO, "GET", "getOrganizationPaymentOptionByNAV", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, nav)));

        // flip entity to model
        PaymentOptionWithDebtorInfoModelResponse paymentOptionResponse = modelMapper.map(
                paymentsService.getPaymentOptionByNAV(organizationFiscalCode, nav),
                PaymentOptionWithDebtorInfoModelResponse.class);

        return new ResponseEntity<>(paymentOptionResponse, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<PaymentOptionModelResponse> payPaymentOption(String organizationFiscalCode, String nav,
                                                                       @Valid PaymentOptionModel paymentOptionModel) {
        log.info(String.format(LOG_BASE_HEADER_INFO, "POST", "payPaymentOption", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, nav)));

        PaymentOption paidPaymentOption = paymentsService.pay(organizationFiscalCode, nav, paymentOptionModel);


        if (null != paidPaymentOption) {
            return new ResponseEntity<>(ObjectMapperUtils.map(paidPaymentOption, PaymentOptionModelResponse.class), HttpStatus.OK);
        }

        throw new AppException(AppError.PAYMENT_OPTION_PAY_FAILED, organizationFiscalCode, nav);
    }

    @Override
    public ResponseEntity<TransferModelResponse> reportTransfer(String organizationFiscalCode, String iuv,
                                                                String transferId) {
        log.info(String.format(LOG_BASE_HEADER_INFO, "POST", "reportTransfer", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iuv) + "; transferId=" + transferId));
        Transfer reportedTransfer = paymentsService.report(organizationFiscalCode, iuv, transferId);
        if (null != reportedTransfer) {
            return new ResponseEntity<>(ObjectMapperUtils.map(reportedTransfer, TransferModelResponse.class), HttpStatus.OK);
        }
        throw new AppException(AppError.TRANSFER_REPORTING_FAILED, organizationFiscalCode, iuv, transferId);
    }

    @Override
    public ResponseEntity<PaymentOptionModelResponse> updateNotificationFee(String organizationFiscalCode, String iuv,
                                                                      NotificationFeeUpdateModel notificationFeeUpdateModel) {
        Long notificationFee = notificationFeeUpdateModel.getNotificationFee();
        log.info(String.format(LOG_BASE_HEADER_INFO, "PUT", "updateNotificationFee", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iuv) + "; notificationFee=" + notificationFee));
        PaymentOption updatedPaymentOption = paymentsService.updateNotificationFee(organizationFiscalCode, iuv, notificationFee);
        if (updatedPaymentOption != null) {
        	ResponseEntity.status(Boolean.FALSE.equals(updatedPaymentOption.isPaymentInProgress())?HttpStatus.OK.value():CustomHttpStatus.IN_PROGRESS.value());
            return ResponseEntity
            		.status(Boolean.FALSE.equals(updatedPaymentOption.isPaymentInProgress())?HttpStatus.OK.value():CustomHttpStatus.IN_PROGRESS.value())
            		.body(ObjectMapperUtils.map(updatedPaymentOption, PaymentOptionModelResponse.class));
        }
        throw new AppException(AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_FAILED, organizationFiscalCode, iuv);
    }

}
