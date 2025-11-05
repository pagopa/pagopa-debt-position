package it.gov.pagopa.debtposition.service.payments;

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
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import it.gov.pagopa.debtposition.model.payments.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.verify.response.InstallmentSummary;
import it.gov.pagopa.debtposition.model.payments.verify.response.PaymentOptionGroup;
import it.gov.pagopa.debtposition.model.payments.verify.response.VerifyPaymentOptionsResponse;
import it.gov.pagopa.debtposition.model.send.response.NotificationPriceResponse;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.extern.slf4j.Slf4j;
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
  private final NodeClient nodeClient;
  private final SendClient sendClient;

  public PaymentsService(
      PaymentPositionRepository paymentPositionRepository,
      PaymentOptionRepository paymentOptionRepository,
      NodeClient nodeClient,
      SendClient sendClient) {
    this.paymentPositionRepository = paymentPositionRepository;
    this.paymentOptionRepository = paymentOptionRepository;
    this.nodeClient = nodeClient;
    this.sendClient = sendClient;
  }

  @Value("${nav.aux.digit}")
  private String auxDigit;

  // TODO #naviuv: temporary regression management --> the nav variable can also be evaluated with
  // iuv. Remove the comment when only nav managment is enabled
  @Transactional
  public PaymentOption getPaymentOptionByNAV(
      @NotBlank String organizationFiscalCode, @NotBlank String nav) {

    Optional<PaymentOption> po =
        paymentOptionRepository.findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            organizationFiscalCode, nav, organizationFiscalCode, nav);

    if (po.isEmpty()) {
      throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav);
    }

    PaymentOption paymentOption = po.get();
    
    // Update PaymentPosition instance only in memory
    // PaymentPosition used when converting PaymentOption to POWithDebtor
    DebtPositionStatus.validityCheckAndUpdate(paymentOption);
    DebtPositionStatus.expirationCheckAndUpdate(paymentOption);
    DebtPositionStatus.checkAlreadyPaidInstallments(paymentOption, nav, paymentOptionRepository);

    // Synchronous update of notification fees
    if (paymentOption.getSendSync()) {
      boolean result = updateNotificationFeeSync(paymentOption);
      if (result)
        log.info(
            "Notification fee amount of Payment Option with NAV {} has been updated with"
                + " notification-fee: {}.",
            paymentOption.getNav(),
            paymentOption.getNotificationFee());
      else
        log.error(
            "[GPD-ERR-SEND-01] Error while updating notification fee amount for NAV {}.",
            paymentOption.getNav());
    }

    return paymentOption;
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

    // Update PaymentPosition instance only in memory
    DebtPositionStatus.validityCheckAndUpdate(paymentPositionToPay);
    DebtPositionValidation.checkPaymentPositionPayability(paymentPositionToPay, nav);

    PaymentOption poToPay = paymentPositionToPay.getPaymentOption().stream()
    	    .filter(po -> nav.equals(po.getNav()) || nav.equals(po.getIuv()))
    	    .findFirst()
    	    .orElseThrow(() -> new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav));
    
    DebtPositionStatus.checkAlreadyPaidInstallments(poToPay, nav, paymentOptionRepository);
    
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
    .orElseThrow(() -> new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, iuv));

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
  
  @Transactional(readOnly = true)
  public VerifyPaymentOptionsResponse verifyPaymentOptions(String organizationFiscalCode, String nav) {
    
	PaymentOption paymentOption = paymentOptionRepository
        .findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            organizationFiscalCode, nav, organizationFiscalCode, nav)
        .orElseThrow(() -> new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav));

    DebtPositionStatus.validityCheckAndUpdate(paymentOption);
    DebtPositionStatus.expirationCheckAndUpdate(paymentOption);

    PaymentPosition pp = paymentOption.getPaymentPosition();
	final boolean ppInvalidOrExpired = 
			pp.getStatus() == DebtPositionStatus.INVALID || pp.getStatus() == DebtPositionStatus.EXPIRED;
    
    List<PaymentOption> siblings = pp.getPaymentOption();

    // Ec info
    VerifyPaymentOptionsResponse.VerifyPaymentOptionsResponseBuilder resp = VerifyPaymentOptionsResponse.builder()
        .organizationFiscalCode(pp.getOrganizationFiscalCode())
        .companyName(pp.getCompanyName())
        .officeName(pp.getOfficeName())
        .standIn(Boolean.TRUE.equals(pp.getPayStandIn()));

    // Group POs into:
    // - "single" (isPartialPayment = false) => groupKey = "SINGLE:<POid>"
    // - "plan"   (isPartialPayment = true)  => groupKey = "PLAN:<paymentPlanId>"
    Map<String, List<PaymentOption>> grouped = siblings.stream().collect(Collectors.groupingBy(po -> {
      if (Boolean.TRUE.equals(po.getIsPartialPayment())) {
        return PLAN + po.getPaymentPlanId();
      } else {
        return SINGLE + po.getId();
      }
    }));
    
    // Find out if there is an "active plan" with an in progress payment
    Optional<String> activePlanKey = findActivePlanKey(grouped);

    List<PaymentOptionGroup> groups = new ArrayList<>();
    for (Map.Entry<String, List<PaymentOption>> e : grouped.entrySet()) {
      List<PaymentOption> list = e.getValue();
      // true if the current group is NOT the active plan (joint obligors rule → INVALID at runtime)
      boolean outsideActivePlan = activePlanKey.map(k -> !k.equals(e.getKey())).orElse(false);
      boolean forceInvalid = ppInvalidOrExpired || outsideActivePlan;

      long totalAmount = list.stream().mapToLong(PaymentOption::getAmount).sum();

      LocalDateTime maxDueDate = list.stream()
          .map(PaymentOption::getDueDate)
          .filter(Objects::nonNull)
          .max(Comparator.naturalOrder())
          .orElse(null);

      LocalDateTime minValidity = list.stream()
          .map(PaymentOption::getValidityDate)
          .filter(Objects::nonNull)
          .min(Comparator.naturalOrder())
          .orElse(null);

      // Description:
      // - For "SINGLE:*" groups -> ALWAYS "Payment in a single installment"
      // - For "PLAN:*" groups   -> "Installment plan of N payments" (fallback: first non-null PO description)
      String groupKey = e.getKey();
      String description;

      if (groupKey.startsWith(SINGLE)) {
    	  // Force a canonical label for single-payment option
    	  description = "Payment in a single installment";
      } else {
    	  int n = list.size();
    	  String fallback = list.stream()
    			  .map(PaymentOption::getDescription)
    			  .filter(Objects::nonNull)
    			  .findFirst()
    			  .orElse("");
    	  description = (n > 0) ? ("Installment plan of " + n + " payments") : fallback;
      }

      // allCCP: true if ALL transfers from ALL POs in the group have a valid postalIBAN (not null/blank)
      boolean allCCP = list.stream()
          .flatMap(po -> po.getTransfer().stream())
          .allMatch(t -> t.getPostalIban() != null && !t.getPostalIban().isBlank());

      /**
       * Aggregate PO status for a group:
       * - if PP is invalid -> all PO is PO_INVALID
       * - else if all PAID -> PO_PAID
       * - else if some PAID & some UNPAID within same group -> PO_PARTIALLY_PAID
       * - else handle EXPIRED flavors using dueDate/validityDate/switchToExpired
       * - else -> PO_UNPAID
       */
      GroupStatus gs = aggregateGroupPoStatus(list, ppInvalidOrExpired, forceInvalid); 

      // installments sorted by dueDate
      List<InstallmentSummary> installments = list.stream()
          .sorted(Comparator.comparing(PaymentOption::getDueDate, Comparator.nullsLast(Comparator.naturalOrder())))
          .map(po -> InstallmentSummary.builder()
              .nav(po.getNav())
              .iuv(po.getIuv())
              .amount(po.getAmount())
              .description(po.getDescription())
              .dueDate(po.getDueDate())
              .validFrom(po.getValidityDate())
              .status(toInstallmentStatus(po, ppInvalidOrExpired, forceInvalid))       
              .statusReason(toInstallmentReason(po, ppInvalidOrExpired, forceInvalid)) // optional: detail text
              .build())
          .toList();

      PaymentOptionGroup group = PaymentOptionGroup.builder()
          .description(description)
          .numberOfInstallments(list.size())
          .amount(totalAmount)
          .dueDate(maxDueDate)
          .validFrom(minValidity)
          .status(gs.status)              // PO_INVALID / PO_UNPAID / PO_PAID / PO_PARTIALLY_PAID / PO_EXPIRED_*
          .statusReason(gs.reason)        // optional free text
          .allCCP(allCCP)
          .installments(installments)
          .build();

      groups.add(group);
    }

    // order by duedate
    groups.sort(Comparator.comparing(PaymentOptionGroup::getDueDate, Comparator.nullsLast(Comparator.naturalOrder())));

    return resp.paymentOptions(groups).build();
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

  public List<OrganizationModelQueryBean> getOrganizationsToAdd(@NotNull LocalDate since) {
    return paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
  }

  public List<OrganizationModelQueryBean> getOrganizationsToDelete(@NotNull LocalDate since) {
    paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
    return Collections.emptyList();
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
    	throw new AppException(AppError.PAYMENT_OPTION_NOT_FOUND, pp.getOrganizationFiscalCode(), nav);
    }

    this.recomputePaymentPositionStatus(pp);
    
    if (pp.getStatus() == DebtPositionStatus.PAID || pp.getStatus() == DebtPositionStatus.REPORTED) {
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
          String error = String.format("Obtained unexpected empty transfer - [organizationFiscalCode= %s; iupd= %s; iuv= %s; idTransfer= %s]", pp.getOrganizationFiscalCode(), pp.getIupd(), iuv, transferId);
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
   * Recompute PaymentPosition status (V3 semantics):
   * - A "plan" is:
   *   - a group of POs with the same paymentPlanId (isPartialPayment = TRUE), or
   *   - a single non-installment PO (isPartialPayment = FALSE).
   * - The PP becomes:
   *   - REPORTED if at least one plan is fully REPORTED;
   *   - otherwise PAID if at least one plan is fully {PAID|REPORTED};
   *   - otherwise PARTIALLY_PAID if a subset of installments is paid/reported;
   *   - otherwise the status remains unchanged.
   */
   private void recomputePaymentPositionStatus(PaymentPosition pp) {
	   Map<String, List<PaymentOption>> groups = pp.getPaymentOption().stream()
			    .collect(Collectors.groupingBy(PaymentsService::groupKeyOf));

     boolean anyPlanInProgress = false;    // there is at least one installment paid/reported/partially reported 
     boolean anyPlanFullyPaid = false;     // there is a fully {PAID|REPORTED} plan
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
       if (allPaidOrReported)   anyPlanFullyPaid = true;
       if (allReported)         anyPlanFullyReported = true;
     }

     if (anyPlanFullyReported) {
       pp.setStatus(DebtPositionStatus.REPORTED);
     } else if (anyPlanFullyPaid) {
       pp.setStatus(DebtPositionStatus.PAID);
     } else if (anyPlanInProgress) {
       pp.setStatus(DebtPositionStatus.PARTIALLY_PAID);
     }
   }
  
  private static class GroupStatus {
	  final String status;
	  final String reason;
	  GroupStatus(String status, String reason) { this.status = status; this.reason = reason; }
	}

	/**
	 * Aggregate PO status for a group:
	 * - if PP is invalid -> all PO is PO_INVALID
	 * - else if all PAID -> PO_PAID
	 * - else if some PAID & some UNPAID within same group -> PO_PARTIALLY_PAID
	 * - else handle EXPIRED flavors using dueDate/validityDate/switchToExpired
	 * - else -> PO_UNPAID
	 */
  private GroupStatus aggregateGroupPoStatus(List<PaymentOption> list, boolean ppInvalid, boolean forceInvalid) {

	  if (ppInvalid || forceInvalid) {
		  return new GroupStatus("PO_INVALID", ppInvalid ? "Debt position is INVALID or EXPIRED" : "Not payable: another payment option has already been used");
	  }

	  boolean allPaid = list.stream().allMatch(po -> po.getStatus() == PaymentOptionStatus.PO_PAID);
	  if (allPaid) return new GroupStatus("PO_PAID", "All installments have been paid");

	  boolean anyPaid   = list.stream().anyMatch(po -> po.getStatus() == PaymentOptionStatus.PO_PAID);
	  boolean anyUnpaid = list.stream().anyMatch(po -> po.getStatus() == PaymentOptionStatus.PO_UNPAID);

	  if (anyPaid && anyUnpaid) {
		  return new GroupStatus("PO_PARTIALLY_PAID", "Some installments already paid");
	  }

	  LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);

	  // all expired?
	  boolean allExpired = list.stream().allMatch(po -> isExpired(po, now));
	  if (allExpired) {
		  // NOT_PAYABLE if at least one PO has switchToExpired=true
		  boolean anyNotPayable = list.stream().anyMatch(po -> Boolean.TRUE.equals(po.getSwitchToExpired()));
		  return anyNotPayable
				  ? new GroupStatus("PO_EXPIRED_NOT_PAYABLE", "Group expired and not payable")
						  : new GroupStatus("PO_EXPIRED_UNPAID", "Group expired but still payable");
	  }

	  // default
	  return new GroupStatus("PO_UNPAID", "No installment has been paid");
  }

	private boolean isExpired(PaymentOption po, LocalDateTime now) {
		LocalDateTime due = po.getDueDate();
		if (due == null) return false;
		return now.isAfter(due);
	}

	private String toInstallmentStatus(PaymentOption po, boolean ppInvalid, boolean forceInvalid) {

		if (ppInvalid || forceInvalid) return "POI_INVALID";

		if (po.getStatus() == PaymentOptionStatus.PO_PAID)    return "POI_PAID";

		LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
		boolean expired = isExpired(po, now);

		if (expired) {
			boolean notPayable = Boolean.TRUE.equals(po.getSwitchToExpired());
			return notPayable ? "POI_EXPIRED_NOT_PAYABLE" : "POI_EXPIRED_UNPAID";
		}

		return "POI_UNPAID";
	}

	private String toInstallmentReason(PaymentOption po, boolean ppInvalid, boolean forceInvalid) {
		if (ppInvalid) return "Debt position is INVALID or EXPIRED";
		if (forceInvalid) return "Not payable: another payment option has already been used";
		if (po.getStatus() == PaymentOptionStatus.PO_PAID)    return null;

		LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
		boolean expired = isExpired(po, now);

		if (expired) {
			boolean notPayable = Boolean.TRUE.equals(po.getSwitchToExpired());
			return notPayable ? "Expired and not payable" : "Expired but payable";
		}
		return null;
	}
	
	/** flag to identify a payment in progress */
	private static boolean hasProgress(PaymentOption po) {
	  return po.getStatus() == PaymentOptionStatus.PO_PAID
	      || po.getStatus() == PaymentOptionStatus.PO_PARTIALLY_REPORTED
	      || po.getStatus() == PaymentOptionStatus.PO_REPORTED;
	}

	/** Grouping key: PLAN:<paymentPlanId> or SINGLE:<id> */
	private static String groupKeyOf(PaymentOption po) {
	  if (Boolean.TRUE.equals(po.getIsPartialPayment())) {
	    return PLAN + po.getPaymentPlanId();
	  } else {
	    return SINGLE + po.getId();
	  }
	}

	/** Check if there is an "active plan," that is, an installment plan that is already being paid. */
	private static Optional<String> findActivePlanKey(Map<String, List<PaymentOption>> groups) {
	  return groups.entrySet().stream()
	      .filter(e -> e.getValue().stream().anyMatch(PaymentsService::hasProgress))
	      .map(Map.Entry::getKey)
	      .findFirst();
	}
}
