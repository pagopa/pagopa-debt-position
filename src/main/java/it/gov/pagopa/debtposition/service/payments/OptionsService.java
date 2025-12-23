package it.gov.pagopa.debtposition.service.payments;

import static it.gov.pagopa.debtposition.service.common.ExpirationHandler.*;
import static it.gov.pagopa.debtposition.service.common.ValidityHandler.*;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.payments.verify.response.InstallmentSummary;
import it.gov.pagopa.debtposition.model.payments.verify.response.PaymentOptionGroup;
import it.gov.pagopa.debtposition.model.payments.verify.response.VerifyPaymentOptionsResponse;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class OptionsService {

  private static final String PLAN = "PLAN:";
  private static final String SINGLE = "SINGLE:";
  private final PaymentOptionRepository paymentOptionRepository;

  public OptionsService(PaymentOptionRepository paymentOptionRepository) {
    this.paymentOptionRepository = paymentOptionRepository;
  }

  @Transactional(readOnly = true)
  public VerifyPaymentOptionsResponse verifyPaymentOptions(
      String organizationFiscalCode, String nav) {

    PaymentOption paymentOption =
        paymentOptionRepository
            .findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
                organizationFiscalCode, nav, organizationFiscalCode, nav)
            .orElseThrow(
                () ->
                    new AppException(
                        AppError.PAYMENT_OPTION_NOT_FOUND, organizationFiscalCode, nav));

    PaymentPosition pp = paymentOption.getPaymentPosition();

    handlePaymentPositionValidTransition(pp);
    handlePaymentPositionExpirationLogic(pp);

    final boolean ppInvalidOrExpired =
        pp.getStatus() == DebtPositionStatus.INVALID
            || pp.getStatus() == DebtPositionStatus.EXPIRED;

    List<PaymentOption> siblings = pp.getPaymentOption();

    // Ec info
    VerifyPaymentOptionsResponse.VerifyPaymentOptionsResponseBuilder resp =
        VerifyPaymentOptionsResponse.builder()
            .organizationFiscalCode(pp.getOrganizationFiscalCode())
            .companyName(pp.getCompanyName())
            .officeName(pp.getOfficeName())
            .standIn(Boolean.TRUE.equals(pp.getPayStandIn()));

    // Group POs into:
    // - "single" (isPartialPayment = false) => groupKey = "SINGLE:<POid>"
    // - "plan"   (isPartialPayment = true)  => groupKey = "PLAN:<paymentPlanId>"
    Map<String, List<PaymentOption>> grouped =
        siblings.stream()
            .collect(
                Collectors.groupingBy(
                    po -> {
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

      long totalAmount = list.stream().mapToLong(PaymentOption::getAmount).sum();

      LocalDateTime maxDueDate =
          list.stream()
              .map(PaymentOption::getDueDate)
              .filter(Objects::nonNull)
              .max(Comparator.naturalOrder())
              .orElse(null);

      LocalDateTime minValidity =
          list.stream()
              .map(PaymentOption::getValidityDate)
              .filter(Objects::nonNull)
              .min(Comparator.naturalOrder())
              .orElse(null);

      String groupKey = e.getKey();
      String description = buildGroupDescription(list);
      // true if the current group is NOT the active plan
      boolean outsideActivePlan = activePlanKey.map(k -> !k.equals(groupKey)).orElse(false);
      boolean forceInvalid = ppInvalidOrExpired || outsideActivePlan;

      // allCCP: true if ALL transfers from ALL POs in the group have a valid postalIBAN (not
      // null/blank)
      boolean allCCP =
          list.stream()
              .flatMap(po -> po.getTransfer().stream())
              .allMatch(t -> t.getPostalIban() != null && !t.getPostalIban().isBlank());

      /**
       * Aggregate PO status for a group: - if PP is invalid -> all PO is PO_INVALID - else if all
       * PAID -> PO_PAID - else if some PAID & some UNPAID within same group -> PO_PARTIALLY_PAID -
       * else handle EXPIRED flavors using dueDate/validityDate/switchToExpired - else -> PO_UNPAID
       */
      GroupStatus gs = aggregateGroupPoStatus(list, ppInvalidOrExpired, forceInvalid);

      List<InstallmentSummary> installments =
          toInstallmentSummaries(list, ppInvalidOrExpired, forceInvalid);

      PaymentOptionGroup group =
          PaymentOptionGroup.builder()
              .description(description)
              .numberOfInstallments(list.size())
              .amount(totalAmount)
              .dueDate(maxDueDate)
              .validFrom(minValidity)
              .status(
                  gs.status) // PO_INVALID / PO_UNPAID / PO_PAID / PO_PARTIALLY_PAID / PO_EXPIRED_*
              .statusReason(gs.reason) // optional free text
              .allCCP(allCCP)
              .installments(installments)
              .build();

      groups.add(group);
    }

    // order by duedate
    groups.sort(
        Comparator.comparing(
            PaymentOptionGroup::getDueDate, Comparator.nullsLast(Comparator.naturalOrder())));

    return resp.paymentOptions(groups).build();
  }

  private static class GroupStatus {
    final String status;
    final String reason;

    GroupStatus(String status, String reason) {
      this.status = status;
      this.reason = reason;
    }
  }

  /**
   * Aggregate PO status for a group: - if PP is invalid -> all PO is PO_INVALID - else if all PAID
   * -> PO_PAID - else if some PAID & some UNPAID within same group -> PO_PARTIALLY_PAID - else
   * handle EXPIRED flavors using dueDate/validityDate/switchToExpired - else -> PO_UNPAID
   */
  private GroupStatus aggregateGroupPoStatus(
      List<PaymentOption> list, boolean ppInvalid, boolean forceInvalid) {

    if (ppInvalid || forceInvalid) {
      return new GroupStatus(
          "PO_INVALID",
          ppInvalid
              ? "Debt position is INVALID or EXPIRED"
              : "Not payable: another payment option has already been used");
    }

    boolean allPaid = list.stream().allMatch(po -> po.getStatus() == PaymentOptionStatus.PO_PAID);
    if (allPaid) return new GroupStatus("PO_PAID", "All installments have been paid");

    boolean anyPaid = list.stream().anyMatch(po -> po.getStatus() == PaymentOptionStatus.PO_PAID);
    boolean anyUnpaid =
        list.stream().anyMatch(po -> po.getStatus() == PaymentOptionStatus.PO_UNPAID);

    if (anyPaid && anyUnpaid) {
      return new GroupStatus("PO_PARTIALLY_PAID", "Some installments already paid");
    }

    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);

    // all expired?
    boolean allExpired = list.stream().allMatch(po -> isPastDueDate(po, now));
    if (allExpired) {
      // NOT_PAYABLE if at least one PO has switchToExpired=true
      boolean anyNotPayable =
          list.stream().anyMatch(po -> Boolean.TRUE.equals(po.getSwitchToExpired()));
      return anyNotPayable
          ? new GroupStatus("PO_EXPIRED_NOT_PAYABLE", "Group expired and not payable")
          : new GroupStatus("PO_EXPIRED_UNPAID", "Group expired but still payable");
    }

    // default
    return new GroupStatus("PO_UNPAID", "No installment has been paid");
  }

  private String toInstallmentStatus(PaymentOption po, boolean ppInvalid, boolean forceInvalid) {

    if (ppInvalid || forceInvalid) return "POI_INVALID";

    if (po.getStatus() == PaymentOptionStatus.PO_PAID) return "POI_PAID";

    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    boolean expired = isPastDueDate(po, now);

    if (expired) {
      boolean notPayable = Boolean.TRUE.equals(po.getSwitchToExpired());
      return notPayable ? "POI_EXPIRED_NOT_PAYABLE" : "POI_EXPIRED_UNPAID";
    }

    return "POI_UNPAID";
  }

  private String toInstallmentReason(PaymentOption po, boolean ppInvalid, boolean forceInvalid) {
    if (ppInvalid) return "Debt position is INVALID or EXPIRED";
    if (forceInvalid) return "Not payable: another payment option has already been used";
    if (po.getStatus() == PaymentOptionStatus.PO_PAID) return null;

    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    boolean expired = isPastDueDate(po, now);

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

  /**
   * Check if there is an "active plan," that is, an installment plan that is already being paid.
   */
  private static Optional<String> findActivePlanKey(Map<String, List<PaymentOption>> groups) {
    return groups.entrySet().stream()
        .filter(e -> e.getValue().stream().anyMatch(OptionsService::hasProgress))
        .map(Map.Entry::getKey)
        .findFirst();
  }

  /**
   * Builds the group description: - SINGLE -> paymentOptionDescription if present, otherwise null -
   * PLAN -> paymentOptionDescription if present, otherwise null
   */
  private static String buildGroupDescription(List<PaymentOption> list) {
    return list.stream()
        .map(PaymentOption::getPaymentOptionDescription)
        .filter(s -> s != null && !s.isBlank())
        .findFirst()
        .orElse(null);
  }

  /** Converts the group's POs to InstallmentSummaries sorted by dueDate. */
  private List<InstallmentSummary> toInstallmentSummaries(
      List<PaymentOption> list, boolean ppInvalidOrExpired, boolean forceInvalid) {
    return list.stream()
        .sorted(
            Comparator.comparing(
                PaymentOption::getDueDate, Comparator.nullsLast(Comparator.naturalOrder())))
        .map(
            po ->
                InstallmentSummary.builder()
                    .nav(po.getNav())
                    .iuv(po.getIuv())
                    .amount(po.getAmount())
                    .description(po.getDescription())
                    .dueDate(po.getDueDate())
                    .validFrom(po.getValidityDate())
                    .status(toInstallmentStatus(po, ppInvalidOrExpired, forceInvalid))
                    .statusReason(toInstallmentReason(po, ppInvalidOrExpired, forceInvalid))
                    .build())
        .toList();
  }
}
