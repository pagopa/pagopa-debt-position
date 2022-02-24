//package it.gov.pagopa.payments.endpoint;
//
//import javax.xml.datatype.DatatypeConfigurationException;
//
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.mockito.Mockito;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.boot.test.mock.mockito.MockBean;
//import org.springframework.http.MediaType;
//import org.springframework.test.context.ContextConfiguration;
//import org.springframework.test.context.junit.jupiter.SpringExtension;
//import org.springframework.test.util.ReflectionTestUtils;
//import org.springframework.test.web.reactive.server.WebTestClient;
//
//import it.gov.pagopa.payments.PaymentsApplication;
//import it.gov.pagopa.payments.config.WebServicesConfiguration;
//import it.gov.pagopa.payments.controller.PaymentsController;
//import it.gov.pagopa.payments.endpoints.validation.PaymentValidator;
//import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
//import it.gov.pagopa.payments.mock.PaVerifyPaymentNoticeResMock;
//import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;
//import it.gov.pagopa.payments.repository.DebitorRepository;
//import it.gov.pagopa.payments.repository.IncrementalIuvNumberRepository;
//import it.gov.pagopa.payments.repository.PaymentPositionRepository;
//import it.gov.pagopa.payments.service.PartnerService;
//import it.gov.pagopa.payments.service.PaymentService;
//
//@ExtendWith(SpringExtension.class)
//@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
//@ContextConfiguration(classes = { WebServicesConfiguration.class, PaymentsApplication.class, PaymentValidator.class })
//class PartnerXsdValidationTest {
//
//    @Autowired
//    private WebTestClient webClient;
//
//    @MockBean
//    private PartnerService partnerService;
//
//    @MockBean
//    private PaymentService paymentService;
//
//    @MockBean
//    private PaymentsController paymentsController;
//
//    @MockBean
//    private DebitorRepository debitorRepository;
//
//    @MockBean
//    private IncrementalIuvNumberRepository incrementalIuvNumberRepository;
//
//    @MockBean
//    private PaymentPositionRepository paymentPositionRepository;
//
//    private final static String PAA_SINTASSI_XSD = "PAA_SINTASSI_XSD";
//
//    @Test
//    void shouldGetWsdlTest() throws Exception {
//        ReflectionTestUtils.setField(paymentsController, "entePath", "");
//        this.webClient.get().uri("/partner/partner.wsdl").exchange().expectStatus().isOk();
//    }
//
//    @Test
//    void shouldXsdValiationErrorWithPaVerifyPaymentNoticeTest() throws DatatypeConfigurationException {
//        ReflectionTestUtils.setField(paymentsController, "entePath", "");
//        String invalidRequest = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:paf=\"http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd\"><soapenv:Header/><soapenv:Body><paf:paVerifyPaymentNoticeReq><idPA>?</idPA><idBrokerPA>1</idBrokerPA><idStation>1</idStation><qrCode><fiscalCode>1</fiscalCode><noticeNumber>1</noticeNumber></qrCode></paf:paVerifyPaymentNoticeReq></soapenv:Body></soapenv:Envelope>";
//
//        this.webClient.post().uri("/partner").header("SOAPAction", "paVerifyPaymentNotice")
//                .contentType(MediaType.TEXT_XML).bodyValue(invalidRequest).exchange().expectStatus().isOk()
//                .expectBody(String.class).returnResult().getResponseBody().contains(PAA_SINTASSI_XSD);
//    }
//
//    @Test
//    void shouldGenericErrorWithPaVerifyPaymentNoticeTest() throws DatatypeConfigurationException {
//
//        Mockito.when(partnerService.paVerifyPaymentNotice(Mockito.any()))
//                .thenThrow(DatatypeConfigurationException.class);
//
//        String request = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:paf=\"http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd\"><soapenv:Header/><soapenv:Body><paf:paVerifyPaymentNoticeReq><idPA>77777777777</idPA><idBrokerPA>77777777777</idBrokerPA><idStation>77777777777</idStation><qrCode><fiscalCode>77777777777</fiscalCode><noticeNumber>311111111112222222</noticeNumber></qrCode></paf:paVerifyPaymentNoticeReq></soapenv:Body></soapenv:Envelope>";
//
//        this.webClient.post().uri("/partner").header("SOAPAction", "paVerifyPaymentNotice")
//                .contentType(MediaType.TEXT_XML).bodyValue(request).exchange().expectStatus().is5xxServerError();
//    }
//
//    @Test
//    void shouldGenericErrorExceptionWithPaVerifyPaymentNoticeTest()
//            throws PartnerValidationException, DatatypeConfigurationException {
//
//        Mockito.when(partnerService.paVerifyPaymentNotice(Mockito.any())).thenThrow(RuntimeException.class);
//
//        String request = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:paf=\"http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd\"><soapenv:Header/><soapenv:Body><paf:paVerifyPaymentNoticeReq><idPA>77777777777</idPA><idBrokerPA>77777777777</idBrokerPA><idStation>77777777777</idStation><qrCode><fiscalCode>77777777777</fiscalCode><noticeNumber>311111111112222222</noticeNumber></qrCode></paf:paVerifyPaymentNoticeReq></soapenv:Body></soapenv:Envelope>";
//
//        this.webClient.post().uri("/partner").header("SOAPAction", "paVerifyPaymentNotice")
//                .contentType(MediaType.TEXT_XML).bodyValue(request).exchange().expectStatus().is5xxServerError();
//    }
//
//    @Test
//    void shouldReturnvalidResponseWithPaVerifyPaymentNoticeTest()
//            throws PartnerValidationException, DatatypeConfigurationException {
//
//        PaVerifyPaymentNoticeRes responseBody = PaVerifyPaymentNoticeResMock.getMock();
//
//        Mockito.when(partnerService.paVerifyPaymentNotice(Mockito.any())).thenReturn(responseBody);
//
//        String request = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:paf=\"http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd\"><soapenv:Header/><soapenv:Body><paf:paVerifyPaymentNoticeReq><idPA>88888888888</idPA><idBrokerPA>77777777777</idBrokerPA><idStation>77777777777_1</idStation><qrCode><fiscalCode>88888888888</fiscalCode><noticeNumber>382202100000000101</noticeNumber></qrCode></paf:paVerifyPaymentNoticeReq></soapenv:Body></soapenv:Envelope>";
//
//        this.webClient.post().uri("/partner").header("SOAPAction", "paVerifyPaymentNotice")
//                .contentType(MediaType.TEXT_XML).bodyValue(request).exchange().expectStatus().is2xxSuccessful();
//    }
//
//    @Test
//    void shouldXsdValiationErrorWithPaGetPaymentTest() throws DatatypeConfigurationException {
//        ReflectionTestUtils.setField(paymentsController, "entePath", "");
//        String invalidRequest = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:paf=\"http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd\"><soapenv:Header/><soapenv:Body><paf:paGetPaymentReq><idPA>?</idPA><idBrokerPA>1</idBrokerPA><idStation>1</idStation><qrCode><fiscalCode>1</fiscalCode><noticeNumber>1</noticeNumber></qrCode></paf:paGetPaymentReq></soapenv:Body></soapenv:Envelope>";
//
//        this.webClient.post().uri("/partner").header("SOAPAction", "paGetPayment").contentType(MediaType.TEXT_XML)
//                .bodyValue(invalidRequest).exchange().expectStatus().isOk().expectBody(String.class).returnResult()
//                .getResponseBody().contains(PAA_SINTASSI_XSD);
//    }
//
//    @Test
//    void shouldXsdValiationErrorWithPaSendRTTest() throws DatatypeConfigurationException {
//        ReflectionTestUtils.setField(paymentsController, "entePath", "");
//        String inRequest = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:paf=\"http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd\"><soapenv:Header/><soapenv:Body><paf:paSendRTReq><idPA>?</idPA><idBrokerPA>1</idBrokerPA><idStation>1</idStation><qrCode><fiscalCode>1</fiscalCode><noticeNumber>1</noticeNumber></qrCode></paf:paSendRTReq></soapenv:Body></soapenv:Envelope>";
//
//        this.webClient.post().uri("/partner").header("SOAPAction", "paSendRT").contentType(MediaType.TEXT_XML)
//                .bodyValue(inRequest).exchange().expectStatus().isOk().expectBody(String.class).returnResult()
//                .getResponseBody().contains(PAA_SINTASSI_XSD);
//    }
//}
