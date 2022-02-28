package it.gov.pagopa.payments.endpoints.validation;

import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import it.gov.pagopa.payments.model.partner.ObjectFactory;
import org.apache.catalina.filters.ExpiresFilter;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.springframework.boot.test.context.SpringBootTest;

import javax.servlet.ServletOutputStream;
import javax.servlet.WriteListener;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest
class SoapMessageDispatcherTest {


    @InjectMocks
    @Spy
    SoapMessageDispatcher soapMessageDispatcher;

    @Mock
    HttpServletRequest request;

    @Mock
    HttpServletResponse response;

    @Mock
    private ObjectFactory factory;

    private final ObjectFactory factoryUtil = new ObjectFactory();

    @Test
    void paVerifyPaymentNotice() throws Exception {
        when(factory.createCtFaultBean()).thenReturn(factoryUtil.createCtFaultBean());
        when(factory.createPaVerifyPaymentNoticeRes()).thenReturn(factoryUtil.createPaVerifyPaymentNoticeRes());
        when(factory.createPaVerifyPaymentNoticeRes(any())).thenReturn(factoryUtil.createPaVerifyPaymentNoticeRes(factoryUtil.createPaVerifyPaymentNoticeRes()));
        when(factory.createCtPaymentOptionDescriptionPA()).thenReturn(factoryUtil.createCtPaymentOptionDescriptionPA());
        when(factory.createCtPaymentOptionsDescriptionListPA())
                .thenReturn(factoryUtil.createCtPaymentOptionsDescriptionListPA());

        when(request.getHeader("SOAPAction")).thenReturn("paVerifyPaymentNotice");
        when(response.getOutputStream()).thenReturn(outputStream);

        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_SEMANTICA))
                .when(soapMessageDispatcher).callService(any(), any());

        soapMessageDispatcher.doService(request, response);

        verify(response, times(1)).getOutputStream();
    }

    @Test
    void paGetPayment() throws Exception {
        when(factory.createCtFaultBean()).thenReturn(factoryUtil.createCtFaultBean());
        when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
        when(factory.createPaGetPaymentRes(any())).thenReturn(factoryUtil.createPaGetPaymentRes(factoryUtil.createPaGetPaymentRes()));

        when(request.getHeader("SOAPAction")).thenReturn("paGetPayment");
        when(response.getOutputStream()).thenReturn(outputStream);

        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_SEMANTICA))
                .when(soapMessageDispatcher).callService(any(), any());

        soapMessageDispatcher.doService(request, response);

        verify(response, times(1)).getOutputStream();
    }

    @Test
    void paSendRT() throws Exception {
        when(factory.createCtFaultBean()).thenReturn(factoryUtil.createCtFaultBean());
        when(factory.createPaSendRTRes()).thenReturn(factoryUtil.createPaSendRTRes());
        when(factory.createPaSendRTRes(any())).thenReturn(factoryUtil.createPaSendRTRes(factoryUtil.createPaSendRTRes()));

        when(request.getHeader("SOAPAction")).thenReturn("paSendRT");
        when(response.getOutputStream()).thenReturn(outputStream);

        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_SEMANTICA))
                .when(soapMessageDispatcher).callService(any(), any());

        soapMessageDispatcher.doService(request, response);

        verify(response, times(1)).getOutputStream();
    }


    @Test
    void doServiceDefault() throws Exception {
        when(factory.createCtFaultBean()).thenReturn(factoryUtil.createCtFaultBean());
        when(factory.createPaSendRTRes()).thenReturn(factoryUtil.createPaSendRTRes());
        when(factory.createPaSendRTRes(any())).thenReturn(factoryUtil.createPaSendRTRes(factoryUtil.createPaSendRTRes()));

        when(request.getHeader("SOAPAction")).thenReturn("unknown");
        when(response.getOutputStream()).thenReturn(outputStream);

        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_SEMANTICA))
                .when(soapMessageDispatcher).callService(any(), any());

        soapMessageDispatcher.doService(request, response);

        verify(response, times(1)).getOutputStream();
    }



    public static final ServletOutputStream outputStream = new ServletOutputStream() {
        @Override
        public boolean isReady() {
            return false;
        }

        @Override
        public void setWriteListener(WriteListener listener) {

        }

        @Override
        public void write(int b) throws IOException {

        }
    };
}
