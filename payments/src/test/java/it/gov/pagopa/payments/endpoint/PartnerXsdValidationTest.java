package it.gov.pagopa.payments.endpoint;

import it.gov.pagopa.payments.PaymentsApplication;
import it.gov.pagopa.payments.config.WebServicesConfiguration;
import it.gov.pagopa.payments.endpoints.validation.PaymentValidator;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.mock.PaVerifyPaymentNoticeResMock;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;
import it.gov.pagopa.payments.service.PartnerService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.reactive.server.WebTestClient;

import javax.xml.datatype.DatatypeConfigurationException;

@ExtendWith(SpringExtension.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@ContextConfiguration(classes = {WebServicesConfiguration.class, PaymentsApplication.class, PaymentValidator.class})
class PartnerXsdValidationTest {

    @Autowired
    private WebTestClient webClient;

    @MockBean
    private PartnerService partnerService;


    @Test
    void shouldGenericErrorWithPaVerifyPaymentNoticeTest() throws DatatypeConfigurationException {

        Mockito.when(partnerService.paVerifyPaymentNotice(Mockito.any()))
                .thenThrow(DatatypeConfigurationException.class);

        String request = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:paf=\"http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd\"><soapenv:Header/><soapenv:Body><paf:paVerifyPaymentNoticeReq><idPA>77777777777</idPA><idBrokerPA>77777777777</idBrokerPA><idStation>77777777777</idStation><qrCode><fiscalCode>77777777777</fiscalCode><noticeNumber>311111111112222222</noticeNumber></qrCode></paf:paVerifyPaymentNoticeReq></soapenv:Body></soapenv:Envelope>";

        this.webClient.post().uri("/partner").header("SOAPAction", "paVerifyPaymentNotice")
                .contentType(MediaType.TEXT_XML).bodyValue(request).exchange()
                .expectStatus().is5xxServerError();
    }

    @Test
    void shouldGenericErrorExceptionWithPaVerifyPaymentNoticeTest()
            throws PartnerValidationException, DatatypeConfigurationException {

        Mockito.when(partnerService.paVerifyPaymentNotice(Mockito.any())).thenThrow(RuntimeException.class);

        String request = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:paf=\"http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd\"><soapenv:Header/><soapenv:Body><paf:paVerifyPaymentNoticeReq><idPA>77777777777</idPA><idBrokerPA>77777777777</idBrokerPA><idStation>77777777777</idStation><qrCode><fiscalCode>77777777777</fiscalCode><noticeNumber>311111111112222222</noticeNumber></qrCode></paf:paVerifyPaymentNoticeReq></soapenv:Body></soapenv:Envelope>";

        this.webClient.post().uri("/partner").header("SOAPAction", "paVerifyPaymentNotice")
                .contentType(MediaType.TEXT_XML).bodyValue(request).exchange().expectStatus().is5xxServerError();
    }

    @Test
    void shouldReturnValidResponseWithPaVerifyPaymentNoticeTest()
            throws PartnerValidationException, DatatypeConfigurationException {

        PaVerifyPaymentNoticeRes responseBody = PaVerifyPaymentNoticeResMock.getMock();

        Mockito.when(partnerService.paVerifyPaymentNotice(Mockito.any())).thenReturn(responseBody);

        String request = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:paf=\"http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd\"><soapenv:Header/><soapenv:Body><paf:paVerifyPaymentNoticeReq><idPA>88888888888</idPA><idBrokerPA>77777777777</idBrokerPA><idStation>77777777777_1</idStation><qrCode><fiscalCode>88888888888</fiscalCode><noticeNumber>382202100000000101</noticeNumber></qrCode></paf:paVerifyPaymentNoticeReq></soapenv:Body></soapenv:Envelope>";

        this.webClient.post().uri("/partner").header("SOAPAction", "paVerifyPaymentNotice")
                .contentType(MediaType.TEXT_XML).bodyValue(request).exchange().expectStatus().is2xxSuccessful();
    }
}
