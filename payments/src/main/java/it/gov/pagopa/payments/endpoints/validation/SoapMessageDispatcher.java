package it.gov.pagopa.payments.endpoints.validation;

import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.partner.CtFaultBean;
import it.gov.pagopa.payments.model.partner.CtResponse;
import it.gov.pagopa.payments.model.partner.ObjectFactory;
import it.gov.pagopa.payments.model.partner.PaDemandPaymentNoticeResponse;
import it.gov.pagopa.payments.model.partner.PaGetPaymentRes;
import it.gov.pagopa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;
import it.gov.pagopa.payments.model.partner.StOutcome;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.ws.transport.http.MessageDispatcherServlet;
import org.w3c.dom.Document;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import java.io.IOException;

@Component
@Slf4j
public class SoapMessageDispatcher extends MessageDispatcherServlet {

    private static final long serialVersionUID = 2735436671084580797L;

    @Autowired
    private ObjectFactory factory;

    private static final String SOAP_PREFIX = "soapenv";

    @Value("${pt.id_intermediario}")
    private String intermediario;


    @Override
    protected void doService(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) {

        String faultCode = null;
        String faultString = null;
        String description = null;
        PaVerifyPaymentNoticeRes paVerifyPaymentNoticeRes = null;
        JAXBElement<PaVerifyPaymentNoticeRes> paVerifyPaymentNoticeResJaxbElement = null;
        PaGetPaymentRes paGetPaymentRes = null;
        JAXBElement<PaGetPaymentRes> paGetPaymentResJaxbElement = null;
        PaSendRTRes paSendRTRes = null;
        JAXBElement<PaSendRTRes> paSendRTResJaxbElement = null;
        PaDemandPaymentNoticeResponse paDemandPaymentNoticeResponse = null;
        JAXBElement<PaDemandPaymentNoticeResponse> paDemandPaymentNoticeResponseJaxbElement = null;
        CtResponse ctResponse = null;

        String soapAction = httpServletRequest.getHeader("SOAPAction") != null
                ? httpServletRequest.getHeader("SOAPAction").replace("\"", "")
                : null;

        try {
            callService(httpServletRequest, httpServletResponse);
        } catch (PartnerValidationException e) {

            log.error("Processing resulted in exception: " + e.getMessage());
            faultCode = e.getError().getFaultCode();
            faultString = e.getError().getFaultString();
            description = e.getError().getDescription();
            httpServletResponse.setStatus(200);

        } catch (Exception e) {

            log.error("Processing resulted in generic exception: " + e.getMessage());
            httpServletResponse.setStatus(500);
        }

        if (faultCode != null && soapAction != null) {

            CtFaultBean faultBean = factory.createCtFaultBean();
            faultBean.setDescription(description);
            faultBean.setFaultCode(faultCode);
            faultBean.setFaultString(faultString);
            faultBean.setId(intermediario);

            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);

            try {
                Document doc = dbf.newDocumentBuilder().newDocument();

                switch (soapAction) {
                    case "paVerifyPaymentNotice":
                        paVerifyPaymentNoticeRes = factory.createPaVerifyPaymentNoticeRes();
                        paVerifyPaymentNoticeRes.setOutcome(StOutcome.KO);
                        paVerifyPaymentNoticeRes.setFault(faultBean);
                        paVerifyPaymentNoticeResJaxbElement = factory.createPaVerifyPaymentNoticeRes(paVerifyPaymentNoticeRes);
                        JAXBContext.newInstance(PaVerifyPaymentNoticeRes.class).createMarshaller()
                                .marshal(paVerifyPaymentNoticeResJaxbElement, doc);
                        break;
                    case "paGetPayment":
                        paGetPaymentRes = factory.createPaGetPaymentRes();
                        paGetPaymentRes.setOutcome(StOutcome.KO);
                        paGetPaymentRes.setFault(faultBean);
                        paGetPaymentResJaxbElement = factory.createPaGetPaymentRes(paGetPaymentRes);
                        JAXBContext.newInstance(PaGetPaymentRes.class).createMarshaller()
                                .marshal(paGetPaymentResJaxbElement, doc);
                        break;
                    case "paSendRT":
                        paSendRTRes = factory.createPaSendRTRes();
                        paSendRTRes.setOutcome(StOutcome.KO);
                        paSendRTRes.setFault(faultBean);
                        paSendRTResJaxbElement = factory.createPaSendRTRes(paSendRTRes);
                        JAXBContext.newInstance(PaSendRTRes.class).createMarshaller().marshal(paSendRTResJaxbElement,
                                doc);
                        break;
                    case "paDemandPaymentNotice":
                        paDemandPaymentNoticeResponse = factory.createPaDemandPaymentNoticeResponse();
                        paDemandPaymentNoticeResponse.setOutcome(StOutcome.KO);
                        paDemandPaymentNoticeResponse.setFault(faultBean);
                        paDemandPaymentNoticeResponseJaxbElement = factory.createPaDemandPaymentNoticeResponse(paDemandPaymentNoticeResponse);
                        JAXBContext.newInstance(PaDemandPaymentNoticeResponse.class).createMarshaller().marshal(paDemandPaymentNoticeResponseJaxbElement,
                                doc);
                        break;
                    default:
                        ctResponse = factory.createPaSendRTRes();
                        ctResponse.setOutcome(StOutcome.KO);
                        ctResponse.setFault(faultBean);
                        paSendRTResJaxbElement = factory.createPaSendRTRes(paSendRTRes);
                        JAXBContext.newInstance(PaSendRTRes.class).createMarshaller().marshal(paSendRTResJaxbElement, doc);
                        break;
                }

                SOAPMessage soapMessage = MessageFactory.newInstance().createMessage();

                soapMessage.getSOAPPart().getEnvelope().removeNamespaceDeclaration("SOAP-ENV");
                soapMessage.getSOAPPart().getEnvelope().setPrefix(SOAP_PREFIX);
                soapMessage.getSOAPHeader().setPrefix(SOAP_PREFIX);
                soapMessage.getSOAPBody().setPrefix(SOAP_PREFIX);

                SOAPBody soapBody = soapMessage.getSOAPBody();
                soapBody.addDocument(doc);
                soapMessage.saveChanges();

                ServletOutputStream outputStream = httpServletResponse.getOutputStream();
                httpServletResponse.setContentType("text/xml");
                soapMessage.writeTo(outputStream);
                outputStream.flush();

            } catch (ParserConfigurationException | SOAPException | JAXBException | IOException e) {

                log.error("Processing resulted in generic exception: " + e.getMessage());
                httpServletResponse.setStatus(500);
            }
        }

    }

    protected void callService(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws Exception {
        super.doService(httpServletRequest, httpServletResponse);
    }
}
