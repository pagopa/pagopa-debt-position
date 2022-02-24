package it.gov.pagopa.payments.endpoints.validation;

import java.io.IOException;
import java.util.Arrays;
import java.util.stream.Collectors;

import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.transform.TransformerException;

import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import org.springframework.ws.context.MessageContext;
import org.springframework.ws.soap.saaj.SaajSoapMessage;
import org.springframework.ws.soap.server.endpoint.interceptor.PayloadValidatingInterceptor;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class SoapValidatingInterceptor extends PayloadValidatingInterceptor {

    private static final String SOAP_PREFIX = "soapenv";

    @Override
    protected boolean handleRequestValidationErrors(MessageContext messageContext, SAXParseException[] errors)
            throws TransformerException {

        if (errors.length > 0) {
            String validationErrorsString = Arrays.stream(errors).map(
                    error -> "[" + error.getLineNumber() + "," + error.getColumnNumber() + "]: " + error.getMessage())
                    .collect(Collectors.joining(" -- "));
            log.error(validationErrorsString);
            throw new PartnerValidationException(PaaErrorEnum.PAA_SINTASSI_XSD);
        }
        return true;
    }

    @Override
    public boolean handleResponse(MessageContext messageContext, Object endpoint) throws IOException, SAXException {

        SaajSoapMessage soapResponse = (SaajSoapMessage) messageContext.getResponse();
        alterSoapEnvelope(soapResponse);
        return super.handleResponse(messageContext, endpoint);
    }

    private void alterSoapEnvelope(SaajSoapMessage soapResponse) {

        try {

            SOAPMessage soapMessage = soapResponse.getSaajMessage();
            soapMessage.getSOAPPart().getEnvelope().removeNamespaceDeclaration("SOAP-ENV");
            soapMessage.getSOAPPart().getEnvelope().setPrefix(SOAP_PREFIX);
            soapMessage.getSOAPHeader().setPrefix(SOAP_PREFIX);
            soapMessage.getSOAPBody().setPrefix(SOAP_PREFIX);
            soapMessage.saveChanges();
        } catch (SOAPException e) {

            log.error("Processing resulted in exception: " + e.getMessage());
        }
    }
}
