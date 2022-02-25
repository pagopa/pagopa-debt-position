package it.gov.pagopa.payments.endpoints.validation;

import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ws.context.MessageContext;
import org.springframework.ws.soap.saaj.SaajSoapMessage;
import org.springframework.ws.soap.server.endpoint.interceptor.PayloadValidatingInterceptor;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

@Slf4j
public class SoapValidatingInterceptor extends PayloadValidatingInterceptor {

    private static final String SOAP_PREFIX = "soapenv";
    private final List<String> amountNodeElements = List.of("paymentAmount", "amount", "transferAmount");

    @Override
    protected boolean handleRequestValidationErrors(MessageContext messageContext, SAXParseException[] errors) {

        if (errors.length > 0) {
            String validationErrorsString = Arrays.stream(errors).map(error -> "[" + error.getLineNumber() + "," + error.getColumnNumber() + "]: " + error.getMessage()).collect(Collectors.joining(" -- "));
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

            fixNumberAmountFormat(soapMessage);

            soapMessage.saveChanges();
        } catch (SOAPException e) {

            log.error("Processing resulted in exception: " + e.getMessage());
        }
    }

    private void fixNumberAmountFormat(SOAPMessage soapMessage) throws SOAPException {
        for (String nodeElement : amountNodeElements) {
            var nodeList = soapMessage.getSOAPBody().getElementsByTagName(nodeElement);
            for (int i = 0; i < nodeList.getLength(); i++) {
                var node = nodeList.item(i);
                if (node != null) {
                    var amount = node.getFirstChild().getNodeValue();
                    DecimalFormat dec = new DecimalFormat("#0.00", DecimalFormatSymbols.getInstance(Locale.US)); // format example: 10.99
                    double number = Double.parseDouble(amount) / 100.0; // amount is in cents (1099)
                    node.getFirstChild().setNodeValue(dec.format(number));
                }
            }
        }
    }
}
