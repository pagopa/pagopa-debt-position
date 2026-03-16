package it.gov.pagopa.debtposition.controller.payments.api.impl;

import static it.gov.pagopa.debtposition.util.Constants.NOTIFICATION_FEE_METADATA_KEY;
import static it.gov.pagopa.debtposition.util.Constants.PO_MARKED_AS_PAID_FIELD_PLACEHOLDER;

import it.gov.pagopa.debtposition.controller.payments.api.IPaymentsController;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.payments.AlreadyPaidPaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.PaidPaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.payments.verify.response.VerifyPaymentOptionsResponse;
import it.gov.pagopa.debtposition.model.pd.NotificationFeeUpdateModel;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.service.payments.OptionsService;
import it.gov.pagopa.debtposition.service.payments.PaymentsService;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.CustomHttpStatus;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import jakarta.validation.Valid;
import java.util.ArrayList;
import java.util.Arrays;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.validation.annotation.Validated;

@Controller
@Validated
@Slf4j
public class PaymentsController implements IPaymentsController {

  private static final String LOG_BASE_HEADER_INFO =
      "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
  private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; nav= %s";
  private final ModelMapper modelMapper;
  private final PaymentsService paymentsService;
  private final OptionsService optionsService;

  public PaymentsController(
      ModelMapper modelMapper, PaymentsService paymentsService, OptionsService optionsService) {
    this.modelMapper = modelMapper;
    this.paymentsService = paymentsService;
    this.optionsService = optionsService;
  }

  @Override
  public ResponseEntity<PaymentOptionWithDebtorInfoModelResponse> getOrganizationPaymentOptionByNAV(
      String organizationFiscalCode, String nav) {
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
        paymentsService.getPaymentOptionByNAV(organizationFiscalCode, nav);

    // Add NOTIFICATION_FEE_METADATA_KEY to response on the fly
    paymentOptionResponse
        .getPaymentOptionMetadata()
        .add(
            PaymentOptionMetadataModelResponse.builder()
                .key(NOTIFICATION_FEE_METADATA_KEY)
                .value(String.valueOf(paymentOptionResponse.getNotificationFee()))
                .build());

    return new ResponseEntity<>(paymentOptionResponse, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<PaidPaymentOptionModel> payPaymentOption(
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

    PaymentOption paidPaymentOption =
        paymentsService.pay(organizationFiscalCode, nav, paymentOptionModel);

    // Convert entity to model
    PaidPaymentOptionModel paidPaymentOptionModel =
        modelMapper.map(paidPaymentOption, PaidPaymentOptionModel.class);

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
    PaymentOption updatedPaymentOption =
        paymentsService.updateNotificationFee(organizationFiscalCode, iuv, notificationFee);
    if (updatedPaymentOption != null) {
      ResponseEntity.status(
          Boolean.FALSE.equals(updatedPaymentOption.isPaymentInProgress())
              ? HttpStatus.OK.value()
              : CustomHttpStatus.IN_PROGRESS.value());
      return ResponseEntity.status(
              Boolean.FALSE.equals(updatedPaymentOption.isPaymentInProgress())
                  ? HttpStatus.OK.value()
                  : CustomHttpStatus.IN_PROGRESS.value())
          .body(ObjectMapperUtils.map(updatedPaymentOption, PaymentOptionModelResponse.class));
    }
    throw new AppException(
        AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_FAILED, organizationFiscalCode, iuv);
  }

  @Override
  public ResponseEntity<PaymentOptionModelResponse> setPaymentOptionAsAlreadyPaid(
      String organizationFiscalCode,
      String nav,
      String segregationCodes,
      AlreadyPaidPaymentOptionModel alreadyPaidPaymentOptionModel) {

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

    PaymentOption paidPaymentOption =
        paymentsService.pay(organizationFiscalCode, nav, paymentOptionModel);

    // Convert entity to model
    PaymentOptionModelResponse paymentOptionModelResponse =
        modelMapper.map(paidPaymentOption, PaymentOptionModelResponse.class);

    if (paymentOptionModelResponse == null) {
      throw new AppException(AppError.PAYMENT_OPTION_PAY_FAILED, organizationFiscalCode, nav);
    }

    return new ResponseEntity<>(paymentOptionModelResponse, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<VerifyPaymentOptionsResponse> verifyPaymentOptions(
      String organizationFiscalCode, String nav, String segregationCodes) {

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    if (segCodes != null && !CommonUtil.isAuthorizedOnNavBySegregationCode(nav, segCodes)) {
      throw new AppException(AppError.DEBT_POSITION_FORBIDDEN_ON_NAV, organizationFiscalCode, nav);
    }

    VerifyPaymentOptionsResponse resp =
        optionsService.verifyPaymentOptions(organizationFiscalCode, nav);
    return ResponseEntity.ok(resp);
  }
}
