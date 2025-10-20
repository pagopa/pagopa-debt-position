package it.gov.pagopa.debtposition.controller.payments.api.impl;

import it.gov.pagopa.debtposition.controller.payments.api.IPaymentsController;
import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.payments.AlreadyPaidPaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.PaidPaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.pd.NotificationFeeUpdateModel;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.service.payments.PaymentsService;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.CustomHttpStatus;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import java.util.ArrayList;
import java.util.Arrays;

import static it.gov.pagopa.debtposition.util.Constants.NOTIFICATION_FEE_METADATA_KEY;
import static it.gov.pagopa.debtposition.util.Constants.PO_MARKED_AS_PAID_FIELD_PLACEHOLDER;

@Controller
@Slf4j
public class PaymentsController implements IPaymentsController {

    private static final String LOG_BASE_HEADER_INFO =
            "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
    private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; nav= %s";
    private final PaymentsService paymentsService;

    @Autowired
    public PaymentsController(PaymentsService paymentsService) {
        this.paymentsService = paymentsService;
    }

    @Override
    public ResponseEntity<PaymentOptionWithDebtorInfoModelResponse> getOrganizationPaymentOptionByNAV(
            String organizationFiscalCode,
            String nav) {
        log.debug(
                String.format(
                        LOG_BASE_HEADER_INFO,
                        "GET",
                        "getOrganizationPaymentOptionByNAV",
                        String.format(
                                LOG_BASE_PARAMS_DETAIL,
                                CommonUtil.sanitize(organizationFiscalCode),
                                CommonUtil.sanitize(nav))));

        // flip entity to model
        PaymentOptionWithDebtorInfoModelResponse paymentOptionResponse =
                ObjectMapperUtils.map(
                        paymentsService.getInstallmentByNav(organizationFiscalCode, nav),
                        PaymentOptionWithDebtorInfoModelResponse.class);

        // Add NOTIFICATION_FEE_METADATA_KEY to response on the fly
        paymentOptionResponse.getPaymentOptionMetadata()
                .add(PaymentOptionMetadataModelResponse.builder()
                        .key(NOTIFICATION_FEE_METADATA_KEY)
                        .value(String.valueOf(paymentOptionResponse.getNotificationFee()))
                        .build());

        return new ResponseEntity<>(paymentOptionResponse, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<PaidPaymentOptionModel> payInstallment(
            String organizationFiscalCode, String nav, @Valid PaymentOptionModel paymentOptionModel) {
        log.debug(
                String.format(
                        LOG_BASE_HEADER_INFO,
                        "POST",
                        "payPaymentOption",
                        String.format(
                                LOG_BASE_PARAMS_DETAIL,
                                CommonUtil.sanitize(organizationFiscalCode),
                                CommonUtil.sanitize(nav))));

        Installment paidInstallment =
                paymentsService.pay(organizationFiscalCode, nav, paymentOptionModel);

        // Convert entity to model
        PaidPaymentOptionModel paidPaymentOptionModel = ObjectMapperUtils.map(paidInstallment, PaidPaymentOptionModel.class);

        // TODO change error
        if (paidPaymentOptionModel == null) {
            throw new AppException(AppError.PAYMENT_OPTION_PAY_FAILED, organizationFiscalCode, nav);
        }

        return new ResponseEntity<>(paidPaymentOptionModel, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<TransferModelResponse> reportTransfer(
            String organizationFiscalCode, String iuv, String transferId) {
        log.debug(
                String.format(
                        LOG_BASE_HEADER_INFO,
                        "POST",
                        "reportTransfer",
                        String.format(
                                LOG_BASE_PARAMS_DETAIL,
                                CommonUtil.sanitize(organizationFiscalCode),
                                CommonUtil.sanitize(iuv))
                                + "; transferId="
                                + transferId));
        Transfer reportedTransfer = paymentsService.report(organizationFiscalCode, iuv, transferId);
        if (null != reportedTransfer) {
            return new ResponseEntity<>(
                    ObjectMapperUtils.map(reportedTransfer, TransferModelResponse.class), HttpStatus.OK);
        }
        throw new AppException(
                AppError.TRANSFER_REPORTING_FAILED, organizationFiscalCode, iuv, transferId);
    }

    @Override
    public ResponseEntity<PaymentOptionModelResponse> updateNotificationFee(
            String organizationFiscalCode,
            String iuv,
            NotificationFeeUpdateModel notificationFeeUpdateModel) {
        Long notificationFee = notificationFeeUpdateModel.getNotificationFee();
        log.debug(
                String.format(
                        LOG_BASE_HEADER_INFO,
                        "PUT",
                        "updateNotificationFee",
                        String.format(
                                LOG_BASE_PARAMS_DETAIL,
                                CommonUtil.sanitize(organizationFiscalCode),
                                CommonUtil.sanitize(iuv))
                                + "; notificationFee="
                                + notificationFee));
        Installment updatedInstallment =
                paymentsService.updateNotificationFee(organizationFiscalCode, iuv, notificationFee);
        if (updatedInstallment != null) {
            return ResponseEntity.status(
                            OptionType.OPZIONE_UNICA.equals(updatedInstallment.getPaymentOption().getOptionType())
                                    ? HttpStatus.OK.value()
                                    : CustomHttpStatus.IN_PROGRESS.value())
                    .body(ObjectMapperUtils.map(updatedInstallment, PaymentOptionModelResponse.class));
        }
        throw new AppException(
                AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_FAILED, organizationFiscalCode, iuv);
    }

    @Override
    public ResponseEntity<PaymentOptionModelResponse> setPaymentOptionAsAlreadyPaid(
            String organizationFiscalCode, String nav, String segregationCodes, AlreadyPaidPaymentOptionModel alreadyPaidPaymentOptionModel) {

        log.debug(
                String.format(
                        LOG_BASE_HEADER_INFO,
                        "POST",
                        "setPaymentOptionAsAlreadyPaid",
                        String.format(
                                LOG_BASE_PARAMS_DETAIL,
                                CommonUtil.sanitize(organizationFiscalCode),
                                CommonUtil.sanitize(nav))));

        ArrayList<String> segCodes =
                segregationCodes != null
                        ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
                        : null;
        if (segCodes != null && !CommonUtil.isAuthorizedOnNavBySegregationCode(nav, segCodes)) {
            throw new AppException(AppError.DEBT_POSITION_FORBIDDEN_ON_NAV, organizationFiscalCode, nav);
        }

        PaymentOptionModel paymentOptionModel = new PaymentOptionModel();
        paymentOptionModel.setIdReceipt(PO_MARKED_AS_PAID_FIELD_PLACEHOLDER);
        paymentOptionModel.setPspCompany(PO_MARKED_AS_PAID_FIELD_PLACEHOLDER);
        paymentOptionModel.setPaymentDate(alreadyPaidPaymentOptionModel.getPaymentDate());

        Installment paidInstallment =
                paymentsService.pay(organizationFiscalCode, nav, paymentOptionModel);

        // Convert entity to model
        PaymentOptionModelResponse paymentOptionModelResponse = ObjectMapperUtils.map(paidInstallment, PaymentOptionModelResponse.class);

        // TODO change error
        if (paymentOptionModelResponse == null) {
            throw new AppException(AppError.PAYMENT_OPTION_PAY_FAILED, organizationFiscalCode, nav);
        }

        return new ResponseEntity<>(paymentOptionModelResponse, HttpStatus.OK);
    }
}
