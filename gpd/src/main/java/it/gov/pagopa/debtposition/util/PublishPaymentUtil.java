package it.gov.pagopa.debtposition.util;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDateTime;
import java.time.ZoneOffset;

@UtilityClass
@Slf4j
public class PublishPaymentUtil {
    public void publishProcess(PaymentPosition ppToPublish) {
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
                    + "[organizationFiscalCode= " + ppToPublish.getOrganizationFiscalCode() + "; "
                    + "iupd= " + ppToPublish.getIupd() + "; "
                    + "minDueDate= " + ppToPublish.getMinDueDate() + "; "
                    + "request publish date= " + currentDate
                    + "]");
            throw new AppException(AppError.DEBT_POSITION_PUBLISH_DUE_DATE_MISMATCH, ppToPublish.getOrganizationFiscalCode(), ppToPublish.getIupd());
        }
        // Regola 2 - se era stata prevista una data di inizio validità e la richiesta di pubblicazione viene fatta successivamente a tale data viene rilanciato un errore
        else if (ppToPublish.getValidityDate().isBefore(currentDate)) {
            log.error("Publish request occurred after the validity date has expired - "
                    + "[organizationFiscalCode= " + ppToPublish.getOrganizationFiscalCode() + "; "
                    + "iupd= " + ppToPublish.getIupd() + "; "
                    + "validityDate= " + ppToPublish.getValidityDate() + "; "
                    + "request publish date= " + currentDate
                    + "]");
            throw new AppException(AppError.DEBT_POSITION_PUBLISH_VALIDITY_DATE_MISMATCH, ppToPublish.getOrganizationFiscalCode(), ppToPublish.getIupd());
        }
    }
}
