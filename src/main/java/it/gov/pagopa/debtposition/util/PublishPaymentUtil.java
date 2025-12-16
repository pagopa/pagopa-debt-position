package it.gov.pagopa.debtposition.util;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import java.time.LocalDateTime;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

@UtilityClass
@Slf4j
public class PublishPaymentUtil {

  /**
   * This method sets the status of PaymentPosition to PUBLISHED or VALID.
   * The status depends on the validity date:
   * - if all validity dates have a value -> the status will be PUBLISHED;
   * - if one of the validity dates is null -> the status will be VALID;
   * - if all validity dates are null -> the status will be VALID.
   * Validity dates are checked and set to publishDatetime if null.
   *
   * @param ppToPublish PaymentPosition that the user wants to publish
   * @param publishDatetime LocalDateTime when the publish API or workflow is called
   */
  public void publishProcess(PaymentPosition ppToPublish, LocalDateTime publishDatetime) {
    // Validation: it is not possible to PUBLISH a position with a validity date set to null,
    // set forwardToValid if at least one of validity date is updated (ie at least one was null),
    // so defaultValidityDateApplied=true means forward to valid.
    boolean defaultValidityDateApplied = setValidityDateIfAbsent(ppToPublish, publishDatetime);

    ppToPublish.setPublishDate(publishDatetime);
    ppToPublish.setLastUpdatedDate(publishDatetime);

    // If exists at least one valid option the payment position must be in VALID status
    boolean existsValidOption = ppToPublish.getPaymentOption().stream()
            .anyMatch(po -> po.getValidityDate() != null
                    && !po.getValidityDate().isAfter(publishDatetime));

    // If forwardToValid is true, the position directly transitions to the VALID state
    if (defaultValidityDateApplied || existsValidOption) {
      // setValidityDate to ppToPublish remove after v1.1.0 promotion because useless (todo)
      ppToPublish.setValidityDate(publishDatetime);
      ppToPublish.setStatus(DebtPositionStatus.VALID);
    } else {
      // Actual publish process
      ppToPublish.setStatus(DebtPositionStatus.PUBLISHED);
      // Update payment position validity date, status is PUBLISHED (todo)
      ppToPublish.setValidityDate(CommonUtil.resolveMinValidity(ppToPublish));
    }
  }

  // In the hybrid state during deploy there will be po.validityDate=NULL,
  // but they are not actually NULL, they are NULL because we did not transfer them correctly
  private static boolean setValidityDateIfAbsent(PaymentPosition pp, LocalDateTime value) {
    if (pp.getPaymentOption() == null) return false;
    boolean updated = false;
    for (PaymentOption po : pp.getPaymentOption()) {
      if (po.getValidityDate() == null) {
        updated = true;
        po.setValidityDate(value);
      }
    }
    return updated;
  }
}
