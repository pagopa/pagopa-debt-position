package it.gov.pagopa.debtposition.util;

import static it.gov.pagopa.debtposition.util.Constants.UPDATE_ACTION;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ArrayUtils;

@UtilityClass
@Slf4j
public class PublishPaymentUtil {
    public void publishProcess(PaymentPosition ppToPublish, String... action) {
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        ppToPublish.setPublishDate(currentDate);
        ppToPublish.setStatus(DebtPositionStatus.PUBLISHED);
        ppToPublish.setLastUpdatedDate(currentDate);

        // validityDate = min between the validity of the plan installments
        LocalDateTime minValidityDate = CommonUtil.resolveMinValidity(ppToPublish);

        if (minValidityDate == null) {
            // Regola 3 e Regola 4 - se non era stata prevista una data di inizio validità e la data di
            // pubblicazione è < della min_due_date => sovrascrivo lo stato direttamente a VALID
            if (ppToPublish.getMinDueDate().isAfter(currentDate)) {
                ppToPublish.setStatus(DebtPositionStatus.VALID);
                ppToPublish.getPaymentOption().forEach(po -> {
                    if(po.getValidityDate() == null){
                        po.setValidityDate(currentDate);
                        po.getInstallment().forEach(inst ->
                                inst.setStatus(InstallmentStatus.UNPAID)
                        );
                    }
                });
                return;
            }

            // Regola 5 - se la richiesta di pubblicazione è avvenuta dopo che una una delle opzioni di
            // pagamento è scaduta (currentDate > min_due_date) viene rilanciato un errore
            if (ppToPublish.getMinDueDate().isBefore(currentDate)) {
                log.error(
                        "Publish request occurred after the due date of a payment options has expired - "
                                + "[organizationFiscalCode= {}; iupd= {}; minDueDate= {}; request publish date= {}]",
                        CommonUtil.sanitize(ppToPublish.getOrganizationFiscalCode()),
                        CommonUtil.sanitize(ppToPublish.getIupd()),
                        CommonUtil.sanitize(ppToPublish.getMinDueDate().toString()),
                        currentDate);
                throw new AppException(
                        AppError.DEBT_POSITION_PUBLISH_DUE_DATE_MISMATCH,
                        ppToPublish.getOrganizationFiscalCode(),
                        ppToPublish.getIupd());
            }
            return;
        }

        /* (PAGOPA-2459) Regola 2 e 2.bis - se è presente una validityDate:
         *  - Regola 2: NON è un operazione di UPDATE e la richiesta di pubblicazione è successiva alla validityDate --> errore
         *  - Regola 2.bis: è un operazione di UPDATE e la richiesta di pubblicazione è successiva alla validityDate --> stato VALID
         */
        boolean isUpdateAction = !ArrayUtils.isEmpty(action) && UPDATE_ACTION.equalsIgnoreCase(action[0]);

        if (!isUpdateAction && minValidityDate.isBefore(currentDate)) {
            log.error(
                    "Publish request occurred after the validity date expired - [orgFiscalCode={}, iupd={}, validityDate={}, requestPublishDate={}]",
                    CommonUtil.sanitize(ppToPublish.getOrganizationFiscalCode()),
                    CommonUtil.sanitize(ppToPublish.getIupd()),
                    CommonUtil.sanitize(minValidityDate.toString()),
                    currentDate);
            throw new AppException(
                    AppError.DEBT_POSITION_PUBLISH_VALIDITY_DATE_MISMATCH,
                    ppToPublish.getOrganizationFiscalCode(),
                    ppToPublish.getIupd());
        }

        if (isUpdateAction && minValidityDate.isBefore(currentDate)) {
            ppToPublish.setStatus(DebtPositionStatus.VALID);
            ppToPublish.getPaymentOption().forEach(po -> {
                        if(po.getValidityDate().isBefore(currentDate)){
                            po.getInstallment().forEach(inst ->
                                    inst.setStatus(InstallmentStatus.UNPAID)
                            );
                        }
                    }
            );
        }
    }
}