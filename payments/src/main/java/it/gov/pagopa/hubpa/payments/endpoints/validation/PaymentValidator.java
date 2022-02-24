package it.gov.pagopa.hubpa.payments.endpoints.validation;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import it.gov.pagopa.hubpa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.hubpa.payments.entity.PaymentOptions;
import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;
import it.gov.pagopa.hubpa.payments.enumeration.PaymentOptionStatusEnum;
import it.gov.pagopa.hubpa.payments.enumeration.PaymentStatusEnum;
import it.gov.pagopa.hubpa.payments.model.PaaErrorEnum;

@Component
public class PaymentValidator {

    @Value("${pt.id_intermediario}")
    private String ptIdIntermediario;

    @Value("${pt.id_stazione}")
    private String ptIdStazione;

    public void isAuthorize(String ptIdDominioReq, String ptIdIntermediarioReq, String ptIdStazioneReq, String ptIdDominio)
            throws PartnerValidationException {

        if (!ptIdDominioReq.equals(ptIdDominio)) {
            throw new PartnerValidationException(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO);
        }

        if (!ptIdIntermediarioReq.equals(ptIdIntermediario)) {
            throw new PartnerValidationException(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO);
        }

        if (!ptIdStazioneReq.equals(ptIdStazione)) {
            throw new PartnerValidationException(PaaErrorEnum.PAA_STAZIONE_INT_ERRATA);
        }
    }

    public void isPayable(PaymentPosition position, PaymentOptions option) {

        if ((option == null || position == null)
                || (!position.getStatus().equals(PaymentStatusEnum.PUBBLICATO.getStatus())
                        && (!position.getStatus().equals(PaymentStatusEnum.PAGATO_PARZIALE.getStatus())))) {
            throw new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO);
        }

        if (!option.getStatus().equals(PaymentOptionStatusEnum.NON_PAGATO.getStatus())) {
            throw new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO);
        }

    }
}
