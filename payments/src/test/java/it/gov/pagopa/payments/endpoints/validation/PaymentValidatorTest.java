package it.gov.pagopa.payments.endpoints.validation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import it.gov.pagopa.payments.PaymentsApplication;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import it.gov.pagopa.payments.model.creditorinstitution.CreditorInstitutionDetails;
import it.gov.pagopa.payments.service.ApiConfigClient;

@SpringBootTest(classes = PaymentsApplication.class)
class PaymentValidatorTest {

    @MockBean
    //GpdClient gpdClient;
    ApiConfigClient apiConfigClient;

    @Autowired
    @InjectMocks
    PaymentValidator paymentValidator;


    @Test
    void isAuthorize() {
        when(apiConfigClient.getOrganization(anyString())).thenReturn(new CreditorInstitutionDetails());
        
        try {
            paymentValidator.isAuthorize("", "", "");
        } catch (PartnerValidationException e) {
            assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultCode());
            assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getDescription());
            assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultString());
        }
    }
}
