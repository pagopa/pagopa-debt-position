package it.gov.pagopa.payments.service;

import feign.FeignException;
import feign.RetryableException;
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
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import java.io.IOException;
import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
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


    @Test
    void paSendRTTestKOConflict() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        var e = Mockito.mock(FeignException.Conflict.class);
        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenThrow(e);

        try {
            // Test execution
            PaSendRTRes responseBody = partnerService.paSendRT(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test postcondiction
            assertEquals(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO, ex.getError());
        }
    }

    @Test
    void paSendRTTestKORetryableException() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        var e = Mockito.mock(RetryableException.class);
        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenThrow(e);

        try {
            // Test execution
            PaSendRTRes responseBody = partnerService.paSendRT(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test postcondiction
            assertEquals(PaaErrorEnum.PAA_SYSTEM_ERROR, ex.getError());
        }
    }

    @Test
    void paSendRTTestKOFeignException() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        var e = Mockito.mock(FeignException.class);
        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenThrow(e);

        try {
            // Test execution
            PaSendRTRes responseBody = partnerService.paSendRT(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test postcondiction
            assertEquals(PaaErrorEnum.PAA_SEMANTICA, ex.getError());
        }
    }

    @Test
    void paSendRTTestKO() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        var e = Mockito.mock(NullPointerException.class);
        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenThrow(e);

        try {
            // Test execution
            PaSendRTRes responseBody = partnerService.paSendRT(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test postcondiction
            assertEquals(PaaErrorEnum.PAA_SYSTEM_ERROR, ex.getError());
        }
    }


}
