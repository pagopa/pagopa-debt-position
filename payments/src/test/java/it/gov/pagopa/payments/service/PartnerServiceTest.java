package it.gov.pagopa.payments.service;

import it.gov.pagopa.payments.endpoints.validation.PaymentValidator;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.mock.MockUtil;
import it.gov.pagopa.payments.mock.PaGetPaymentReqMock;
import it.gov.pagopa.payments.mock.PaSendRTReqMock;
import it.gov.pagopa.payments.mock.PaVerifyPaymentNoticeReqMock;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import it.gov.pagopa.payments.model.PaymentOptionModel;
import it.gov.pagopa.payments.model.PaymentsModelResponse;
import it.gov.pagopa.payments.model.partner.ObjectFactory;
import it.gov.pagopa.payments.model.partner.PaGetPaymentReq;
import it.gov.pagopa.payments.model.partner.PaGetPaymentRes;
import it.gov.pagopa.payments.model.partner.PaSendRTReq;
import it.gov.pagopa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeReq;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;
import it.gov.pagopa.payments.model.partner.StAmountOption;
import it.gov.pagopa.payments.model.partner.StOutcome;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import java.io.IOException;
import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class PartnerServiceTest {

    @InjectMocks
    private PartnerService partnerService;

    @Mock
    PaymentValidator paymentValidator;

    @Mock
    private ObjectFactory factory;

    @Mock
    private GpdClient gpdClient;

    private final ObjectFactory factoryUtil = new ObjectFactory();

    @Test
    void paVerifyPaymentNoticeTest() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

        when(factory.createPaVerifyPaymentNoticeRes()).thenReturn(factoryUtil.createPaVerifyPaymentNoticeRes());
        when(factory.createCtPaymentOptionDescriptionPA()).thenReturn(factoryUtil.createCtPaymentOptionDescriptionPA());
        when(factory.createCtPaymentOptionsDescriptionListPA())
                .thenReturn(factoryUtil.createCtPaymentOptionsDescriptionListPA());

        when(gpdClient.getPaymentOption(anyString(), anyString()))
                .thenReturn(MockUtil.readModelFromFile("gpd/getPaymentOption.json", PaymentsModelResponse.class));

        // Test execution
        PaVerifyPaymentNoticeRes responseBody = partnerService.paVerifyPaymentNotice(requestBody);

        // Test postcondiction
        assertThat(responseBody.getOutcome()).isEqualTo(StOutcome.OK);
        assertThat(responseBody.getPaymentList().getPaymentOptionDescription().get(0).isAllCCP()).isFalse();
        assertThat(responseBody.getPaymentList().getPaymentOptionDescription().get(0).getAmount())
                .isEqualTo(new BigDecimal(1055));
        assertThat(responseBody.getPaymentList().getPaymentOptionDescription().get(0).getOptions())
                .isEqualTo(StAmountOption.EQ); // de-scoping
        assertThat(responseBody.getFiscalCodePA()).isEqualTo("77777777777");
        assertThat(responseBody.getPaymentDescription()).isEqualTo("string");
    }

    @Test
    void paVerifyPaymentNoticeTestKOConfig() throws DatatypeConfigurationException {

        // Test preconditions
        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO))
                .when(paymentValidator).isAuthorize(anyString(), anyString(), anyString());

        // Test execution
        try {

            partnerService.paVerifyPaymentNotice(requestBody);

        } catch (PartnerValidationException e) {
            // Test postcondiction
            assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultCode());
            assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getDescription());
            assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultString());
        }
    }

//    @Test
//    void paVerifyPaymentNoticeTestKOpagato() throws DatatypeConfigurationException {
//
//        // Test preconditions
//        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();
//        PaymentOptions option = DebitorMock.createPaymentOptionsMock5();
//
//        when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
//                requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));
//
//        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO)).when(paymentValidator)
//                .isPayable(option.getPaymentPosition(), option);
//
//        // Test execution
//        try {
//
//            partnerService.paVerifyPaymentNotice(requestBody);
//
//        } catch (PartnerValidationException e) {
//            // Test postcondiction
//            assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO.getFaultCode());
//            assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO.getDescription());
//            assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO.getFaultString());
//        }
//
//    }
//
//    @Test
//    void paVerifyPaymentNoticeTestKOdominio() throws DatatypeConfigurationException {
//
//        // Test preconditions
//        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();
//        PaymentOptions option = DebitorMock.createPaymentOptionsMock5();
//
//        when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
//                requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));
//
//        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO)).when(paymentValidator).isAuthorize(
//                requestBody.getIdPA(), requestBody.getIdBrokerPA(), requestBody.getIdStation(), option.getFiscalCode());
//
//        // Test execution
//        try {
//
//            partnerService.paVerifyPaymentNotice(requestBody);
//
//        } catch (PartnerValidationException e) {
//            // Test postcondiction
//            assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getFaultCode());
//            assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getDescription());
//            assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getFaultString());
//        }
//
//    }
//
//    @Test
//    void paVerifyPaymentNoticeTestKOintermediario() throws DatatypeConfigurationException {
//
//        // Test preconditions
//        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();
//
//        PaymentOptions option = DebitorMock.createPaymentOptionsMock5();
//
//        when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
//                requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));
//
//        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO)).when(paymentValidator)
//                .isAuthorize(requestBody.getIdPA(), requestBody.getIdBrokerPA(), requestBody.getIdStation(),
//                        option.getFiscalCode());
//
//        // Test execution
//        try {
//
//            partnerService.paVerifyPaymentNotice(requestBody);
//
//        } catch (PartnerValidationException e) {
//            // Test postcondiction
//            assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultCode());
//            assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getDescription());
//            assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultString());
//        }
//
//    }
//
//    @Test
//    void paVerifyPaymentNoticeTestKOstazione() throws DatatypeConfigurationException {
//
//        // Test preconditions
//        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();
//
//        PaymentOptions option = DebitorMock.createPaymentOptionsMock5();
//
//        when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
//                requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));
//
//        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO)).when(paymentValidator).isAuthorize(
//                requestBody.getIdPA(), requestBody.getIdBrokerPA(), requestBody.getIdStation(), option.getFiscalCode());
//
//        // Test execution
//        try {
//
//            partnerService.paVerifyPaymentNotice(requestBody);
//
//        } catch (PartnerValidationException e) {
//            // Test postcondiction
//            assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getFaultCode());
//            assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getDescription());
//            assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_DOMINIO_ERRATO.getFaultString());
//        }
//    }

    @Test
    void paGetPaymentTest() throws PartnerValidationException, DatatypeConfigurationException, IOException {

        // Test preconditions
        PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();

        when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
        when(factory.createCtPaymentPA()).thenReturn(factoryUtil.createCtPaymentPA());
        when(factory.createCtSubject()).thenReturn(factoryUtil.createCtSubject());
        when(factory.createCtEntityUniqueIdentifier()).thenReturn(factoryUtil.createCtEntityUniqueIdentifier());
        when(factory.createCtTransferListPA()).thenReturn(factoryUtil.createCtTransferListPA());
        when(factory.createCtTransferPA()).thenReturn(factoryUtil.createCtTransferPA());

        when(gpdClient.getPaymentOption(anyString(), anyString()))
                .thenReturn(MockUtil.readModelFromFile("gpd/getPaymentOption.json", PaymentsModelResponse.class));

        // Test execution
        PaGetPaymentRes responseBody = partnerService.paGetPayment(requestBody);

        // Test postcondiction
        assertThat(responseBody.getData().getCreditorReferenceId()).isEqualTo("77777777777");
        assertThat(responseBody.getData().getDescription()).isEqualTo("string");
        assertThat(responseBody.getData().getDueDate())
                .isEqualTo(DatatypeFactory.newInstance().newXMLGregorianCalendar("2122-02-24T17:03:59.408"));
        assertThat(responseBody.getData().getRetentionDate())
                .isEqualTo(DatatypeFactory.newInstance().newXMLGregorianCalendar("2022-02-25T17:03:59.408"));
        // TODO add assertThat for debitor, company name and officeName
    }

//    @Test
//    void paGetPaymentwithoutRetentionDateTest() throws PartnerValidationException, DatatypeConfigurationException {
//
//        // Test preconditions
//        PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();
//        PaymentOptions option = DebitorMock.createPaymentOptionsMock4();
//
//        option.setRetentionDate(null);
//
//        when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
//        when(factory.createCtPaymentPA()).thenReturn(factoryUtil.createCtPaymentPA());
//        when(factory.createCtSubject()).thenReturn(factoryUtil.createCtSubject());
//        when(factory.createCtEntityUniqueIdentifier()).thenReturn(factoryUtil.createCtEntityUniqueIdentifier());
//        when(factory.createCtTransferListPA()).thenReturn(factoryUtil.createCtTransferListPA());
//        when(factory.createCtTransferPA()).thenReturn(factoryUtil.createCtTransferPA());
//
//        when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
//                requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));
//
//        // Test execution
//        PaGetPaymentRes responseBody = partnerService.paGetPayment(requestBody);
//
//        // Test postcondiction
//        assertThat(responseBody.getData().getCreditorReferenceId()).isEqualTo(option.getNotificationCode().substring(1));
//        assertThat(responseBody.getData().getRetentionDate()).isNull();
//    }
//
//    @Test
//    void paGetPaymentWithoutIbanTest() throws PartnerValidationException, DatatypeConfigurationException {
//
//        // Test preconditions
//        PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();
//        PaymentOptions option = DebitorMock.createPaymentOptionsMock4();
//
//        option.getTransfers().get(0).setIban(null);
//        option.getTransfers().get(0).setPostalIban(null);
//
//        when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
//        when(factory.createCtPaymentPA()).thenReturn(factoryUtil.createCtPaymentPA());
//        when(factory.createCtSubject()).thenReturn(factoryUtil.createCtSubject());
//        when(factory.createCtEntityUniqueIdentifier()).thenReturn(factoryUtil.createCtEntityUniqueIdentifier());
//        when(factory.createCtTransferListPA()).thenReturn(factoryUtil.createCtTransferListPA());
//        when(factory.createCtTransferPA()).thenReturn(factoryUtil.createCtTransferPA());
//
//        when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
//                requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));
//
//        try {
//            partnerService.paGetPayment(requestBody);
//
//        } catch (PartnerValidationException e) {
//
//            assertTrue(true);
//        }
//    }
//
//    @Test
//    void paGetPaymentWithPostalIbanTest() throws PartnerValidationException, DatatypeConfigurationException {
//
//        // Test preconditions
//        PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();
//        requestBody.setTransferType(StTransferType.POSTAL);
//
//        PaymentOptions option = DebitorMock.createPaymentOptionsMock4();
//
//        when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
//        when(factory.createCtPaymentPA()).thenReturn(factoryUtil.createCtPaymentPA());
//        when(factory.createCtSubject()).thenReturn(factoryUtil.createCtSubject());
//        when(factory.createCtEntityUniqueIdentifier()).thenReturn(factoryUtil.createCtEntityUniqueIdentifier());
//        when(factory.createCtTransferListPA()).thenReturn(factoryUtil.createCtTransferListPA());
//        when(factory.createCtTransferPA()).thenReturn(factoryUtil.createCtTransferPA());
//
//        when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getQrCode().getNoticeNumber(),
//                requestBody.getQrCode().getFiscalCode())).thenReturn(Optional.of(option));
//
//        // Test execution
//        PaGetPaymentRes responseBody = partnerService.paGetPayment(requestBody);
//
//        // Test postcondiction
//        String ibanRes = responseBody.getData().getTransferList().getTransfer().get(0).getIBAN();
//        String ibanTrans1 = option.getTransfers().get(0).getIban();
//        String ibanTrans2 = option.getTransfers().get(1).getIban();
//
//        assertTrue(ibanRes.equals(ibanTrans1) || ibanRes.equals(ibanTrans2));
//    }

    @Test
    void paSendRTTest() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        when(factory.createPaSendRTRes()).thenReturn(factoryUtil.createPaSendRTRes());

        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenReturn(MockUtil.readModelFromFile("gpd/receiptPaymentOption.json", PaymentsModelResponse.class));


        // Test execution
        PaSendRTRes responseBody = partnerService.paSendRT(requestBody);

        // Test postcondiction
        assertThat(responseBody.getOutcome()).isEqualTo(StOutcome.OK);
    }

//    @Test
//    void paSendRTSaveKoTest() throws DatatypeConfigurationException {
//
//        // Test preconditions
//        PaSendRTReq requestBody = PaSendRTReqMock.getMock();
//        PaymentOptions option = DebitorMock.createPaymentOptionsMock4();
//
//        when(paymentOptionRepository.findByNotificationCodeAndFiscalCode(requestBody.getReceipt().getNoticeNumber(),
//                requestBody.getReceipt().getFiscalCode())).thenReturn(Optional.of(option));
//
//        when(paymentOptionRepository.save(any())).thenThrow(IllegalArgumentException.class);
//        // Test execution
//        try {
//
//            partnerService.paSendRT(requestBody);
//
//        } catch (IllegalArgumentException e) {
//
//            assertTrue(true);
//        }
//
//    }

}
