package it.gov.pagopa.payments.endpoints.validation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.util.HashMap;

import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.util.ReflectionTestUtils;

import feign.FeignException;
import feign.Request;
import feign.RequestTemplate;
import it.gov.pagopa.payments.PaymentsApplication;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import it.gov.pagopa.payments.model.creditorinstitution.StationCreditorInstitution;
import it.gov.pagopa.payments.service.ApiConfigClient;

@SpringBootTest(classes = PaymentsApplication.class)
class PaymentValidatorTest {

    @MockBean
    ApiConfigClient apiConfigClient;

    @Autowired
    @InjectMocks
    PaymentValidator paymentValidator;

    @Test
    void isAuthorize_OK() {
        when(apiConfigClient.getOrganization(anyString(), anyString())).thenReturn(new StationCreditorInstitution());

        ReflectionTestUtils.setField(paymentValidator, "ptIdIntermediario", "bbb");
        ReflectionTestUtils.setField(paymentValidator, "ptIdStazione", "ccc");

        paymentValidator.isAuthorize("aaa", "bbb", "ccc");
        // if reach this assert the call was successful
        assertTrue(true);
    }


    @Test
    void isAuthorize_intermediarioFail() {
        when(apiConfigClient.getOrganization(anyString(), anyString())).thenReturn(new StationCreditorInstitution());

        try {
            paymentValidator.isAuthorize("", "", "");
        } catch (PartnerValidationException e) {
            assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultCode());
            assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getDescription());
            assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultString());
        }
    }

    @Test
    void isAuthorize_apiConfigCallFail() {

        ReflectionTestUtils.setField(paymentValidator, "ptIdIntermediario", "bbb");
        ReflectionTestUtils.setField(paymentValidator, "ptIdStazione", "ccc");

        Request request = Request.create(Request.HttpMethod.GET, "/stations/{stationcode}/creditorinstitutions/{creditorinstitutioncode}",
                new HashMap<>(), null, new RequestTemplate());
        when(apiConfigClient.getOrganization(anyString(), anyString())).thenThrow(new FeignException.NotFound("error", request, null, null));

        try {
            paymentValidator.isAuthorize("aaa", "bbb", "ccc");
        } catch (PartnerValidationException e) {
            assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getFaultCode());
        }
    }

}
