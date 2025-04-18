package it.gov.pagopa.debtposition.service.pd.actions;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.service.pd.crud.PaymentPositionCRUDService;
import it.gov.pagopa.debtposition.util.PublishPaymentUtil;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;
import javax.validation.constraints.NotBlank;
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
    long t1 = System.currentTimeMillis();
    PaymentPosition ppToPublish =
        paymentPositionCRUDService.getDebtPositionByIUPD(
            organizationFiscalCode, iupd, segregationCodes);
    long getTime = System.currentTimeMillis() - t1;
    log.debug("getDebtPositionByIUPD elapsed time: " + getTime);

    if (DebtPositionStatus.getPaymentPosNotPublishableStatus().contains(ppToPublish.getStatus())) {
      throw new AppException(AppError.DEBT_POSITION_NOT_PUBLISHABLE, organizationFiscalCode, iupd);
    }
    PublishPaymentUtil.publishProcess(ppToPublish);
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
    DebtPositionStatus.expirationCheckAndUpdate(ppToInvalidate);
    if (DebtPositionStatus.getPaymentPosNotInvalidableStatus()
        .contains(ppToInvalidate.getStatus())) {
      throw new AppException(AppError.DEBT_POSITION_NOT_INVALIDABLE, organizationFiscalCode, iupd);
    }
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    ppToInvalidate.setStatus(DebtPositionStatus.INVALID);
    ppToInvalidate.setLastUpdatedDate(currentDate);
    return paymentPositionRepository.saveAndFlush(ppToInvalidate);
  }
}
