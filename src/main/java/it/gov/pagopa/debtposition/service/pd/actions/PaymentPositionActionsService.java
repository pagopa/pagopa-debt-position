package it.gov.pagopa.debtposition.service.pd.actions;

import static it.gov.pagopa.debtposition.service.common.ExpirationHandler.handlePaymentPositionExpirationLogic;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.service.pd.crud.PaymentPositionCRUDService;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.PublishPaymentUtil;
import jakarta.validation.constraints.NotBlank;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class PaymentPositionActionsService {

  @Autowired private PaymentPositionCRUDService paymentPositionCRUDService;
  @Autowired private PaymentPositionRepository paymentPositionRepository;

  public PaymentPosition publish(
      @NotBlank String organizationFiscalCode,
      @NotBlank String iupd,
      List<String> segregationCodes) {
    PaymentPosition ppToPublish =
        paymentPositionCRUDService.getDebtPositionByIUPD(
            organizationFiscalCode, iupd, segregationCodes);

    if (DebtPositionStatus.getPaymentPosNotPublishableStatus().contains(ppToPublish.getStatus())) {
      throw new AppException(AppError.DEBT_POSITION_NOT_PUBLISHABLE, organizationFiscalCode, iupd);
    }

    publishFlowHandler(ppToPublish, LocalDateTime.now(ZoneOffset.UTC));

    return paymentPositionRepository.saveAndFlush(ppToPublish);
  }

  @Transactional
  public PaymentPosition invalidate(
      @NotBlank String organizationFiscalCode,
      @NotBlank String iupd,
      List<String> segregationCodes) {
    PaymentPosition ppToInvalidate =
        paymentPositionCRUDService.getDebtPositionByIUPD(
            organizationFiscalCode, iupd, segregationCodes);
    handlePaymentPositionExpirationLogic(ppToInvalidate);
    if (DebtPositionStatus.getPaymentPosNotInvalidableStatus()
        .contains(ppToInvalidate.getStatus())) {
      throw new AppException(AppError.DEBT_POSITION_NOT_INVALIDABLE, organizationFiscalCode, iupd);
    }
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    ppToInvalidate.setStatus(DebtPositionStatus.INVALID);
    ppToInvalidate.setLastUpdatedDate(currentDate);
    return paymentPositionRepository.saveAndFlush(ppToInvalidate);
  }

  /**
   * This method handles validation of the current status of the payment position (verifying that is
   * publishable) and calling the publication process.
   *
   * @param ppToPublish PaymentPosition that the user wants to publish
   * @param publishDatetime LocalDateTime when the publish API or workflow is called
   */
  private void publishFlowHandler(PaymentPosition ppToPublish, LocalDateTime publishDatetime) {
    // 0. Due date must be greater than validity date, it's already a property for a debt position
    // created and saved on database
    for (PaymentOption po : ppToPublish.getPaymentOption()) {
      validateDate(
          po.getDueDate(),
          po.getValidityDate(),
          "payment option due_date",
          AppError.DEBT_POSITION_PUBLISH_DUE_DATE_BEFORE_VALIDITY_DATE,
          ppToPublish);
    }

    // 1. Check Min Due Date: minDueDate must be greater than publishDate
    validateDate(
        ppToPublish.getMinDueDate(),
        publishDatetime,
        "min due_date",
        AppError.DEBT_POSITION_PUBLISH_DUE_DATE_MISMATCH,
        ppToPublish);

    // Get minValidityDate: the min value among the validity dates of the plan installments; may be
    // null
    LocalDateTime minValidityDate = CommonUtil.resolveMinValidity(ppToPublish);

    // 2. Check Validity Date: validityDate must be greater or equal than publishDate, null value is
    // valid
    validateDate(
        minValidityDate,
        publishDatetime,
        "validity_date",
        AppError.DEBT_POSITION_PUBLISH_VALIDITY_DATE_MISMATCH,
        ppToPublish);

    // Execute: call publish process
    PublishPaymentUtil.publishProcess(ppToPublish, publishDatetime);
  }

  // ----------- Helper methods -----------

  // Check if dateToCheck >= targetDateTime
  private void validateDate(
      LocalDateTime dateToCheck,
      LocalDateTime targetDateTime,
      String dateDescription,
      AppError appError,
      PaymentPosition pp) {
    // Guard clause: if date is null or in the future, do nothing
    if (dateToCheck == null || targetDateTime == null || !dateToCheck.isBefore(targetDateTime)) {
      return;
    }

    // Pre-sanitize variables for logging
    String safeFiscalCode = CommonUtil.sanitize(pp.getOrganizationFiscalCode());
    String safeIupd = CommonUtil.sanitize(pp.getIupd());
    String safeDateToCheck = CommonUtil.sanitize(dateToCheck.toString());

    // Log the specific error
    log.error(
        "Publish request occurred after the {} has expired - "
            + "[organizationFiscalCode= {}; IUPD= {}; {}= {}; targetDateTime= {}]",
        dateDescription,
        safeFiscalCode,
        safeIupd,
        dateDescription,
        safeDateToCheck,
        targetDateTime);

    // Throw the exception
    throw new AppException(appError, pp.getOrganizationFiscalCode(), pp.getIupd());
  }
}
