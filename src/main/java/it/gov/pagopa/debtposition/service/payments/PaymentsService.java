package it.gov.pagopa.debtposition.service.payments;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
import static it.gov.pagopa.debtposition.service.common.ExpirationHandler.isInstallmentExpired;
import static it.gov.pagopa.debtposition.service.common.PaymentConflictValidator.checkAlreadyPaidInstallments;
import static it.gov.pagopa.debtposition.service.common.ValidityHandler.handlePaymentPositionValidTransition;
import static it.gov.pagopa.debtposition.service.common.ValidityHandler.isInstallmentValid;

import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.extern.slf4j.Slf4j;

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
  private final NotificationFeeUpdateService notificationFeeUpdateService;
  private final PaymentOptionLookupService paymentOptionLookupService;

  public PaymentsService(
      PaymentPositionRepository paymentPositionRepository,
      PaymentOptionRepository paymentOptionRepository,
      ModelMapper modelMapper,
      NodeClient nodeClient,
      SendClient sendClient,
      NotificationFeeUpdateService notificationFeeUpdateService,
      PaymentOptionLookupService paymentOptionLookupService) {
    this.paymentPositionRepository = paymentPositionRepository;
    this.paymentOptionRepository = paymentOptionRepository;
    this.modelMapper = modelMapper;
    this.nodeClient = nodeClient;
    this.sendClient = sendClient;
    this.notificationFeeUpdateService = notificationFeeUpdateService;
    this.paymentOptionLookupService = paymentOptionLookupService;
  }

  @Value("${nav.aux.digit}")
  private String auxDigit;

  public PaymentOptionWithDebtorInfoModelResponse getPaymentOptionByNAV(
      @NotBlank String organizationFiscalCode, @NotBlank String nav) {

	  PaymentOption paymentOption =
			    paymentOptionLookupService.getPaymentOptionByNAVInternal(organizationFiscalCode, nav);

    // Synchronous update of notification fees
    updateNotificationFeeIfSendSync(paymentOption);

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
  
  /**
   * Handles the synchronous notification fee update flow.
   * - Invokes SEND outside the transaction.
   * - Opens a transaction only for saving data
   */
  public boolean updateNotificationFeeSync(PaymentOption paymentOption) {
	  try {
		  // call SEND API to retrieve notification fee amount
		  NotificationPriceResponse sendResponse =
				  sendClient.getNotificationFee(
						  paymentOption.getOrganizationFiscalCode(),
						  paymentOption.getNav());

		  long notificationFeeAmount = sendResponse.getTotalPrice();
          
		  // call transactional service method to execute the update of the notification fee and related amounts 
		  PaymentOption updatedPaymentOption =
				  notificationFeeUpdateService.applyNotificationFeeUpdate(
						  paymentOption.getId(),
						  notificationFeeAmount);

		  paymentOption.setNotificationFee(updatedPaymentOption.getNotificationFee());
		  paymentOption.setAmount(updatedPaymentOption.getAmount());
		  paymentOption.setLastUpdatedDate(updatedPaymentOption.getLastUpdatedDate());
		  paymentOption.setLastUpdatedDateNotificationFee(
				  updatedPaymentOption.getLastUpdatedDateNotificationFee());

		  return true;

	  } catch (Exception e) {
		  log.error(
				  "[GPD-ERR-SEND-00] Exception while calling getNotificationFee for NAV {}, class = {}, message = {}.",
				  paymentOption.getNav(),
				  e.getClass(),
				  e.getMessage());
		  return false;
	  }
  }
  
  /**
   * Handles the notification fee update flow after checking payment status on Nodo.
   * The process is split into three distinct phases:
   * 1. Database Read: Fetch necessary data.
   * 2. HTTP Call to nodeClient.getCheckPosition(...): Executed outside the transaction to avoid holding DB connections during potentially long HTTP calls.
   * 3. Database Update: Persist results back to the DB.
   */
  public PaymentOption updateNotificationFee(
		  @NotBlank String organizationFiscalCode,
		  @NotBlank String nav,
		  Long notificationFeeAmount) {

	  PaymentOptionNotificationFeeContext context =
			  notificationFeeUpdateService.loadContext(
					  organizationFiscalCode,
					  nav);

	  Boolean paymentInProgress =
			    checkPaymentInProgressOnNode(
			        context.organizationFiscalCode(),
			        nav);

	  return notificationFeeUpdateService.applyNotificationFeeUpdate(
			  context.paymentOptionId(),
			  notificationFeeAmount,
			  paymentInProgress);
  }
  
  public record PaymentOptionNotificationFeeContext(
		  Long paymentOptionId,
		  String organizationFiscalCode,
		  String nav
		  ) {}
  
  private void updateNotificationFeeIfSendSync(PaymentOption paymentOption) {
	  if (!Boolean.TRUE.equals(paymentOption.getSendSync())) {
		  return;
	  }

	  String safeNav = CommonUtil.sanitize(paymentOption.getNav());

	  try {
		  if (this.updateNotificationFeeSync(paymentOption)) {
			  log.info(
					  "Notification fee amount of Payment Option with NAV {} has been updated with notification-fee: {}.",
					  safeNav,
					  paymentOption.getNotificationFee());
		  } else {
			  log.error(
					  "[GPD-ERR-SEND-01] Error while updating notification fee amount for NAV {}.",
					  safeNav);
		  }
	  } catch (Exception e) {
		  log.error(
				  "[GPD-ERR-SEND-02] Failed to update notification fee for NAV {}: {}",
				  safeNav,
				  CommonUtil.sanitize(e.getMessage()));
	  }
  }
  
  private Boolean checkPaymentInProgressOnNode(
		  String organizationFiscalCode,
		  String nav) {

	  String safeOrganizationFiscalCode = CommonUtil.sanitize(organizationFiscalCode);
	  String safeNav = CommonUtil.sanitize(nav);

	  try {
		  NodePosition position =
				  NodePosition.builder()
				  .fiscalCode(organizationFiscalCode)
				  .noticeNumber(auxDigit + nav)
				  .build();

		  NodeCheckPositionResponse response =
				  nodeClient.getCheckPosition(
						  NodeCheckPositionModel.builder()
						  .positionslist(Collections.singletonList(position))
						  .build());

		  return !"OK".equalsIgnoreCase(response.getOutcome());

	  } catch (FeignException.BadRequest e) {
		  NodePosition position =
				  NodePosition.builder()
				  .fiscalCode(organizationFiscalCode)
				  .noticeNumber(nav)
				  .build();

		  try {
			  NodeCheckPositionResponse response =
					  nodeClient.getCheckPosition(
							  NodeCheckPositionModel.builder()
							  .positionslist(Collections.singletonList(position))
							  .build());

			  return !"OK".equalsIgnoreCase(response.getOutcome());

		  } catch (Exception ex) {
			  log.error(
					  "Error checking the position on the node for PO with fiscalCode {} and noticeNumber ({}){}",
					  safeOrganizationFiscalCode,
					  auxDigit,
					  safeNav,
					  ex);

			  return Boolean.TRUE;
		  }

	  } catch (Exception e) {
		  log.error(
				  "Error checking the position on the node for PO with fiscalCode {} and noticeNumber ({}){}",
				  safeOrganizationFiscalCode,
				  auxDigit,
				  safeNav,
				  e);

		  return Boolean.TRUE;
	  }
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
