package it.gov.pagopa.payments.endpoints.validation;

import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class PaymentValidator {

    @Value("${pt.id_intermediario}")
    private String ptIdIntermediario;

    @Value("${pt.id_stazione}")
    private String ptIdStazione;

    public void isAuthorize(String ptIdDominioReq, String ptIdIntermediarioReq, String ptIdStazioneReq)
            throws PartnerValidationException {

// TODO check idDominio
//        if (!ptIdDominioReq.equals(ptIdDominio)) {
//            throw new PartnerValidationException(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO);
//        }

        if (!ptIdIntermediario.equals(ptIdIntermediarioReq)) {
            throw new PartnerValidationException(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO);
        }

        if (!ptIdStazione.equals(ptIdStazioneReq)) {
            throw new PartnerValidationException(PaaErrorEnum.PAA_STAZIONE_INT_ERRATA);
        }
    }

}
