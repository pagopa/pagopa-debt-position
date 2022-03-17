package it.gov.pagopa.payments.endpoints.validation;

import feign.FeignException;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import it.gov.pagopa.payments.service.GpdClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class PaymentValidator {

    @Value("${pt.id_intermediario}")
    private String ptIdIntermediario;

    @Value("${pt.id_stazione}")
    private String ptIdStazione;

    @Autowired
    private GpdClient gpdClient;

    public void isAuthorize(String ptIdDominioReq, String ptIdIntermediarioReq, String ptIdStazioneReq)
            throws PartnerValidationException {

        if (!ptIdIntermediario.equals(ptIdIntermediarioReq)) {
            throw new PartnerValidationException(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO);
        }

        if (!ptIdStazione.equals(ptIdStazioneReq)) {
            throw new PartnerValidationException(PaaErrorEnum.PAA_STAZIONE_INT_ERRATA);
        }

        try {
            gpdClient.getOrganization(ptIdDominioReq);
        }  catch (Exception e) {
            if (e.getCause() instanceof FeignException.FeignClientException) {
                throw new PartnerValidationException(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO);
            } else {
                throw new PartnerValidationException(PaaErrorEnum.PAA_SYSTEM_ERROR);
            }
        }
    }

}
