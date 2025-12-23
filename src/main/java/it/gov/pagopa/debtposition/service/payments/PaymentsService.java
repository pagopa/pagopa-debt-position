package it.gov.pagopa.debtposition.service.payments;

import static it.gov.pagopa.debtposition.service.common.ExpirationHandler.*;
import static it.gov.pagopa.debtposition.service.common.PaymentConflictValidator.checkAlreadyPaidInstallments;
import static it.gov.pagopa.debtposition.service.common.ValidityHandler.*;

import feign.FeignException;
import it.gov.pagopa.debtposition.client.NodeClient;
import it.gov.pagopa.debtposition.client.SendClient;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.checkposition.NodeCheckPositionModel;
import it.gov.pagopa.debtposition.model.checkposition.NodePosition;
import it.gov.pagopa.debtposition.model.checkposition.response.NodeCheckPositionResponse;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.send.response.NotificationPriceResponse;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class PaymentsService {

  private static final String PLAN = "PLAN:";
  private static final String SINGLE = "SINGLE:";
  private final PaymentOptionRepository paymentOptionRepository;
  private final PaymentPositionRepository paymentPositionRepository;
  private final ModelMapper modelMapper;
  private final NodeClient nodeClient;
  private final SendClient sendClient;

  public PaymentsService(
      PaymentPositionRepository paymentPositionRepository,
      PaymentOptionRepository paymentOptionRepository,
      ModelMapper modelMapper,
      NodeClient nodeClient,
      SendClient sendClient) {
    this.paymentPositionRepository = paymentPositionRepository;
    this.paymentOptionRepository = paymentOptionRepository;
    this.modelMapper = modelMapper;
    this.nodeClient = nodeClient;
    this.sendClient = sendClient;
  }

  @Value("${nav.aux.digit}")
  private String auxDigit;

  // TODO #naviuv: temporary regression management --> the nav variable can also be evaluated with
  // iuv. Remove the comment when only nav managment is enabled
  @Transactional
  public PaymentOptionWithDebtorInfoModelResponse getPaymentOptionByNAV(
      @NotBlank String organizationFiscalCode, @NotBlank String nav) {

    Optional<PaymentOption> po =
        paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            organizationFiscalCode, nav, organizationFiscalCode, nav);

    if (po.isEmpty()) {
      throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
    }

    PaymentOption paymentOption = po.get();

    // FSM Logic: Update state (PaymentPosition status) based on current time
    handlePaymentPositionValidTransition(paymentOption.getPaymentPosition());
    handlePaymentPositionExpirationLogic(paymentOption.getPaymentPosition());

    checkAlreadyPaidInstallments(paymentOption, nav, paymentOptionRepository);

    // Synchronous update of notification fees
    if (Boolean.TRUE.equals(paymentOption.getSendSync())) {
      if (this.updateNotificationFeeSync(paymentOption)) {
        log.info(
                "Notification fee amount of Payment Option with NAV {} has been updated with"
                        + " notification-fee: {}.",
                paymentOption.getNav(),
                paymentOption.getNotificationFee());
      } else {
        log.error(
                "[GPD-ERR-SEND-01] Error while updating notification fee amount for NAV {}.",
                paymentOption.getNav());
      }
    }

    PaymentOptionWithDebtorInfoModelResponse paymentOptionResponse =
            modelMapper.map(paymentOption, PaymentOptionWithDebtorInfoModelResponse.class);
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);

    // Update only in memory response
    if (isInstallmentExpired(currentDate, paymentOption)) {
      paymentOptionResponse.setDebtPositionStatus(DebtPositionStatus.EXPIRED);
    }

    // Treat installments that have not reached their validity date as non-existent
    if (!isInstallmentValid(currentDate, paymentOption)) {
      throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
    }

    return paymentOptionResponse;
  }

  @Transactional
  public PaymentOption pay(
      @NotBlank String organizationFiscalCode,
      @NotBlank String nav,
      @NotNull @Valid PaymentOptionModel paymentOptionModel) {
    Optional<PaymentPosition> paymentPositionToPayOpt =
        paymentPositionRepository
            .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvOrPaymentOptionOrganizationFiscalCodeAndPaymentOptionNav(
                organizationFiscalCode, nav, organizationFiscalCode, nav);

    if (paymentPositionToPayOpt.isEmpty()) {
      throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
    }

    PaymentPosition paymentPositionToPay = paymentPositionToPayOpt.get();

    // Update PaymentPosition if necessary
    handlePaymentPositionValidTransition(paymentPositionToPay);
    DebtPositionValidation.checkPaymentPositionPayability(paymentPositionToPay, nav);

    PaymentOption poToPay =
        paymentPositionToPay.getPaymentOption().stream()
            .filter(po -> nav.equals(po.getNav()) || nav.equals(po.getIuv()))
            .findFirst()
            .orElseThrow(
                () ->
                    new AppException(
                        AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav));

    checkAlreadyPaidInstallments(poToPay, nav, paymentOptionRepository);

    return this.executePaymentFlow(paymentPositionToPay, nav, paymentOptionModel);
  }

  @Transactional
  public Transfer report(
      @NotBlank String organizationFiscalCode, @NotBlank String iuv, @NotBlank String transferId) {

    Optional<PaymentPosition> ppToReport =
        paymentPositionRepository
            .findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionIuvAndPaymentOptionTransferIdTransfer(
                organizationFiscalCode, iuv, transferId);

    if (ppToReport.isEmpty()) {
      throw new AppException(AppError.TRANSFER_NOT_FOUND, organizationFiscalCode, iuv, transferId);
    }

    DebtPositionValidation.checkPaymentPositionAccountability(ppToReport.get(), iuv, transferId);

    ppToReport.get().getPaymentOption().stream()
        .filter(po -> iuv.equals(po.getIuv()))
        .findFirst()
        .orElseThrow(
            () -> new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, iuv));

    return this.updateTransferStatus(ppToReport.get(), iuv, transferId);
  }

  @Transactional
  public boolean updateNotificationFeeSync(PaymentOption paymentOption) {
    try {
      // call SEND API to retrieve notification fee amount
      NotificationPriceResponse sendResponse =
          sendClient.getNotificationFee(
              paymentOption.getOrganizationFiscalCode(), paymentOption.getNav());
      int notificationFeeAmount = sendResponse.getTotalPrice();
      // call internal method updateAmountsWithNotificationFee
      updateAmountsWithNotificationFee(
          paymentOption, paymentOption.getOrganizationFiscalCode(), notificationFeeAmount);
      // track the PO last update
      paymentOption.setLastUpdatedDate(LocalDateTime.now(ZoneOffset.UTC));
      paymentOption.setLastUpdatedDateNotificationFee(LocalDateTime.now(ZoneOffset.UTC));

      paymentOptionRepository.saveAndFlush(paymentOption);
      return true;
    } catch (Exception e) {
      log.error(
          "[GPD-ERR-SEND-00] Exception while calling getNotificationFee for NAV {}, class = {},"
              + " message = {}.",
          paymentOption.getNav(),
          e.getClass(),
          e.getMessage());
      return false;
    }
  }

  @Transactional
  public PaymentOption updateNotificationFee(
      @NotBlank String organizationFiscalCode, @NotBlank String nav, Long notificationFeeAmount) {

    // Check if exists a payment option with the passed IUV related to the organization
    // TODO #naviuv: temporary regression management: search by nav or iuv
    Optional<PaymentOption> paymentOptionOpt =
        paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            organizationFiscalCode, nav, organizationFiscalCode, nav);
    if (paymentOptionOpt.isEmpty()) {
      throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
    }
    // Check if the retrieved payment option was not already paid and/or reported
    PaymentOption paymentOption = paymentOptionOpt.get();
    if (!PaymentOptionStatus.PO_UNPAID.equals(paymentOption.getStatus())) {
      throw new AppException(
          AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_NOT_UPDATABLE,
          organizationFiscalCode,
          nav);
    }

    // Executing the amount updating with the inserted notification fee
    updateAmountsWithNotificationFee(paymentOption, organizationFiscalCode, notificationFeeAmount);

    // Executes a call to the node's checkPosition API to see if there is a payment in progress
    try {
      // TODO #naviuv: temporary regression management: search by nav or iuv --> possible double
      // call to the node
      // 1. first call attempt is with the nav variable valued as iuv (auxDigit added)
      NodePosition position =
          NodePosition.builder()
              .fiscalCode(organizationFiscalCode)
              .noticeNumber(auxDigit + nav)
              .build();
      NodeCheckPositionResponse chkPositionRes =
          nodeClient.getCheckPosition(
              NodeCheckPositionModel.builder()
                  .positionslist(Collections.singletonList(position))
                  .build());
      paymentOption.setPaymentInProgress(
          "OK".equalsIgnoreCase(chkPositionRes.getOutcome()) ? Boolean.FALSE : Boolean.TRUE);
    } catch (FeignException.BadRequest e) {
      // 2. if the first call fails with a bad request error --> try with a nav call
      NodePosition position =
          NodePosition.builder().fiscalCode(organizationFiscalCode).noticeNumber(nav).build();
      try {
        NodeCheckPositionResponse chkPositionRes =
            nodeClient.getCheckPosition(
                NodeCheckPositionModel.builder()
                    .positionslist(Collections.singletonList(position))
                    .build());
        paymentOption.setPaymentInProgress(
            "OK".equalsIgnoreCase(chkPositionRes.getOutcome()) ? Boolean.FALSE : Boolean.TRUE);
      } catch (Exception ex) {
        log.error(
            "Error checking the position on the node for PO with fiscalCode "
                + organizationFiscalCode
                + " and noticeNumber "
                + "("
                + auxDigit
                + ")"
                + nav,
            ex);
        // By business rules it is expected to treat the error as if the node had responded KO
        paymentOption.setPaymentInProgress(Boolean.TRUE);
      }
    } catch (Exception e) {
      log.error(
          "Error checking the position on the node for PO with fiscalCode "
              + organizationFiscalCode
              + " and noticeNumber "
              + "("
              + auxDigit
              + ")"
              + nav,
          e);
      // By business rules it is expected to treat the error as if the node had responded KO
      paymentOption.setPaymentInProgress(Boolean.TRUE);
    }

    // Updated to track the PO update
    paymentOption.setLastUpdatedDate(LocalDateTime.now(ZoneOffset.UTC));
    paymentOption.setLastUpdatedDateNotificationFee(LocalDateTime.now(ZoneOffset.UTC));

    paymentOptionRepository.saveAndFlush(paymentOption);
    return paymentOption;
  }

  public static void updateAmountsWithNotificationFee(
      PaymentOption paymentOption, String organizationFiscalCode, long notificationFeeAmount) {
    // Get the first valid transfer to add the fee
    Transfer validTransfer = findPrimaryTransfer(paymentOption, organizationFiscalCode);

    /*
    Retrieving the old notification fee. It MUST BE SUBTRACTED from the various amount in order due to the fact that
    these values were updated in a previous step with another value and adding the new value directly can cause miscalculations.
     */
    long oldNotificationFee = Optional.ofNullable(paymentOption.getNotificationFee()).orElse(0L);

    // Setting the new value of the notification fee, updating the amount of the payment option and
    // the last updated date fee
    paymentOption.setNotificationFee(notificationFeeAmount);
    paymentOption.setAmount(paymentOption.getAmount() - oldNotificationFee);
    paymentOption.setAmount(paymentOption.getAmount() + notificationFeeAmount);

    // Subtracting the old value and adding the new one
    validTransfer.setAmount(validTransfer.getAmount() - oldNotificationFee);
    validTransfer.setAmount(validTransfer.getAmount() + notificationFeeAmount);
  }

  /**
   * find the primary transfer of the payment option
   *
   * @param paymentOption the entity of the payment option
   * @param organizationFiscalCode EC
   * @return the transfer of the primary Creditor Institution
   */
  public static Transfer findPrimaryTransfer(
      PaymentOption paymentOption, String organizationFiscalCode) {
    List<Transfer> transfers = paymentOption.getTransfer();
    return transfers.stream()
        .sorted(Comparator.comparing(Transfer::getIdTransfer))
        .filter(
            transfer ->
                transfer.getOrganizationFiscalCode() == null
                    || organizationFiscalCode.equals(transfer.getOrganizationFiscalCode()))
        .findFirst()
        .orElseThrow(
            () ->
                new AppException(
                    AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_TRANSFER_NOT_FOUND,
                    paymentOption.getIuv(),
                    organizationFiscalCode));
  }

  private PaymentOption executePaymentFlow(
      PaymentPosition pp, String nav, PaymentOptionModel paymentOptionModel) {

    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    PaymentOption paidPO = null;

    for (PaymentOption po : pp.getPaymentOption()) {
      // TODO #naviuv: temporary regression management --> remove "|| po.getIuv().equals(nav)" when
      // only nav managment is enabled
      if (po.getNav().equals(nav) || po.getIuv().equals(nav)) {
        po.setLastUpdatedDate(currentDate);
        po.setPaymentDate(paymentOptionModel.getPaymentDate());
        po.setPaymentMethod(paymentOptionModel.getPaymentMethod());
        po.setPspCode(paymentOptionModel.getPspCode());
        po.setPspTaxCode(paymentOptionModel.getPspTaxCode());
        po.setPspCompany(paymentOptionModel.getPspCompany());
        po.setIdReceipt(paymentOptionModel.getIdReceipt());
        po.setFee(Long.parseLong(paymentOptionModel.getFee()));
        po.setStatus(PaymentOptionStatus.PO_PAID);
        paidPO = po;
        break; // IMPORTANTE
      }
    }
    if (paidPO == null) {
      throw new AppException(
          AppError.PAYMENT_OPTION_NOT_FOUND, pp.getOrganizationFiscalCode(), nav);
    }

    this.recomputePaymentPositionStatus(pp);

    if (pp.getStatus() == DebtPositionStatus.PAID
        || pp.getStatus() == DebtPositionStatus.REPORTED) {
      pp.setPaymentDate(paymentOptionModel.getPaymentDate());
    }

    pp.setLastUpdatedDate(currentDate);

    // salvo l'aggiornamento del pagamento
    paymentPositionRepository.saveAndFlush(pp);
    return paidPO;
  }

  private Transfer updateTransferStatus(PaymentPosition pp, String iuv, String transferId) {

    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    long numberPOTransfers = 0;
    long countReportedTransfer = 0;
    Transfer reportedTransfer = null;

    for (PaymentOption po : pp.getPaymentOption()) {

      if (po.getIuv().equals(iuv)) {
        // numero totale dei transfer per la PO
        numberPOTransfers = po.getTransfer().stream().count();
        // numero dei transfer della PO in stato T_REPORTED
        countReportedTransfer =
            po.getTransfer().stream()
                .filter(t -> t.getStatus().equals(TransferStatus.T_REPORTED))
                .count();
        // recupero il transfer oggetto di rendicontazione
        Optional<Transfer> transferToReport =
            po.getTransfer().stream().filter(t -> t.getIdTransfer().equals(transferId)).findFirst();

        if (transferToReport.isEmpty()) {
          String error =
              String.format(
                  "Obtained unexpected empty transfer - [organizationFiscalCode= %s; iupd= %s; iuv="
                      + " %s; idTransfer= %s]",
                  pp.getOrganizationFiscalCode(), pp.getIupd(), iuv, transferId);
          throw new AppException(AppError.TRANSFER_REPORTING_FAILED, error);
        }

        transferToReport.get().setStatus(TransferStatus.T_REPORTED);
        transferToReport.get().setLastUpdatedDate(currentDate);
        countReportedTransfer++;
        // aggiorno lo stato della PO
        if (countReportedTransfer < numberPOTransfers) {
          po.setStatus(PaymentOptionStatus.PO_PARTIALLY_REPORTED);
        } else {
          po.setStatus(PaymentOptionStatus.PO_REPORTED);
        }
        po.setLastUpdatedDate(currentDate);

        reportedTransfer = transferToReport.get();
      }
    }

    this.recomputePaymentPositionStatus(pp);
    pp.setLastUpdatedDate(currentDate);
    // salvo l'aggiornamento della rendicontazione
    paymentPositionRepository.saveAndFlush(pp);

    return reportedTransfer;
  }

  /**
   * Recompute PaymentPosition status (V3 semantics): - A "plan" is: - a group of POs with the same
   * paymentPlanId (isPartialPayment = TRUE), or - a single non-installment PO (isPartialPayment =
   * FALSE). - The PP becomes: - REPORTED if at least one plan is fully REPORTED; - otherwise PAID
   * if at least one plan is fully {PAID|REPORTED}; - otherwise PARTIALLY_PAID if a subset of
   * installments is paid/reported; - otherwise the status remains unchanged.
   */
  private void recomputePaymentPositionStatus(PaymentPosition pp) {
    Map<String, List<PaymentOption>> groups =
        pp.getPaymentOption().stream().collect(Collectors.groupingBy(PaymentsService::groupKeyOf));

    boolean anyPlanInProgress =
        false; // there is at least one installment paid/reported/partially reported
    boolean anyPlanFullyPaid = false; // there is a fully {PAID|REPORTED} plan
    boolean anyPlanFullyReported = false; // there is a fully REPORTED plan

    for (List<PaymentOption> group : groups.values()) {
      boolean allPaidOrReported = true;
      boolean allReported = true;
      boolean groupHasAnyProgress = false;

      for (PaymentOption po : group) {

        if (po.getStatus() == PaymentOptionStatus.PO_PAID
            || po.getStatus() == PaymentOptionStatus.PO_REPORTED
            || po.getStatus() == PaymentOptionStatus.PO_PARTIALLY_REPORTED) {
          groupHasAnyProgress = true;
        }

        // fully paid if all installments of a plan are {PAID | PARTIALLY_REPORTED | REPORTED}
        if (!(po.getStatus() == PaymentOptionStatus.PO_PAID
            || po.getStatus() == PaymentOptionStatus.PO_PARTIALLY_REPORTED
            || po.getStatus() == PaymentOptionStatus.PO_REPORTED)) {
          allPaidOrReported = false;
        }

        // fully accounted for if all installments are REPORTED
        if (po.getStatus() != PaymentOptionStatus.PO_REPORTED) {
          allReported = false;
        }
      }

      if (groupHasAnyProgress) anyPlanInProgress = true;
      if (allPaidOrReported) anyPlanFullyPaid = true;
      if (allReported) anyPlanFullyReported = true;
    }

    if (anyPlanFullyReported) {
      pp.setStatus(DebtPositionStatus.REPORTED);
    } else if (anyPlanFullyPaid) {
      pp.setStatus(DebtPositionStatus.PAID);
    } else if (anyPlanInProgress) {
      pp.setStatus(DebtPositionStatus.PARTIALLY_PAID);
    }
  }

  /** Grouping key: PLAN:<paymentPlanId> or SINGLE:<id> */
  private static String groupKeyOf(PaymentOption po) {
    if (Boolean.TRUE.equals(po.getIsPartialPayment())) {
      return PLAN + po.getPaymentPlanId();
    } else {
      return SINGLE + po.getId();
    }
  }
}
