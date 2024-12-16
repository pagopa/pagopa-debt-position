package it.gov.pagopa.debtposition.util;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import static it.gov.pagopa.debtposition.util.Constants.UPDATE_ACTION;


import java.time.LocalDateTime;
import java.time.ZoneOffset;

import org.apache.commons.lang3.ArrayUtils;

@UtilityClass
@Slf4j
public class PublishPaymentUtil {
    public void publishProcess(PaymentPosition ppToPublish, String... action) {
        LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
        ppToPublish.setPublishDate(currentDate);
        ppToPublish.setStatus(DebtPositionStatus.PUBLISHED);
        ppToPublish.setLastUpdatedDate(currentDate);
        // Regola 3 e Regola 4 - se non era stata prevista una data di inizio validità e la data di pubblicazione è < della min_due_date => sovrascrivo lo stato direttamente a VALID 
        if (null == ppToPublish.getValidityDate() && ppToPublish.getMinDueDate().isAfter(currentDate)) {	
        	ppToPublish.setValidityDate(currentDate);
            ppToPublish.setStatus(DebtPositionStatus.VALID);
        }
        // Regola 5 - se la richiesta di pubblicazione è avvenuta dopo che una una delle opzioni di pagamento è scaduta (currentDate > min_due_date) viene rilanciato un errore
        else if (null == ppToPublish.getValidityDate() && ppToPublish.getMinDueDate().isBefore(currentDate)) {
            log.error("Publish request occurred after the due date of a payment options has expired - "
                    + "[organizationFiscalCode= " + CommonUtil.sanitize(ppToPublish.getOrganizationFiscalCode()) + "; "
                    + "iupd= " + CommonUtil.sanitize(ppToPublish.getIupd()) + "; "
                    + "minDueDate= " + CommonUtil.sanitize(ppToPublish.getMinDueDate().toString()) + "; "
                    + "request publish date= " + currentDate
                    + "]");
            throw new AppException(AppError.DEBT_POSITION_PUBLISH_DUE_DATE_MISMATCH, ppToPublish.getOrganizationFiscalCode(), ppToPublish.getIupd());
        }
        
        /* (PAGOPA-2459) Regola 2 e 2.bis - se è presente una validityDate:
        *  - Regola 2: NON è un operazione di UPDATE e la richiesta di pubblicazione è successiva alla validityDate --> errore
        *  - Regola 2.bis: è un operazione di UPDATE e la richiesta di pubblicazione è successiva alla validityDate --> stato VALID
        */
        else if (ppToPublish.getValidityDate() != null) {
            boolean isUpdateAction = !ArrayUtils.isEmpty(action) && UPDATE_ACTION.equalsIgnoreCase(action[0]);
            
            if (!isUpdateAction && ppToPublish.getValidityDate().isBefore(currentDate)) {
                log.error("Publish request occurred after the validity date expired - [orgFiscalCode={}, iupd={}, validityDate={}, requestPublishDate={}]",
                		CommonUtil.sanitize(ppToPublish.getOrganizationFiscalCode()),
                		CommonUtil.sanitize(ppToPublish.getIupd()),
                		CommonUtil.sanitize(ppToPublish.getValidityDate().toString()),
                        currentDate
                );
                throw new AppException(AppError.DEBT_POSITION_PUBLISH_VALIDITY_DATE_MISMATCH,
                        ppToPublish.getOrganizationFiscalCode(),
                        ppToPublish.getIupd());
            }

            if (isUpdateAction && ppToPublish.getValidityDate().isBefore(currentDate)) {
                ppToPublish.setStatus(DebtPositionStatus.VALID);
            }
        }
    }
}
