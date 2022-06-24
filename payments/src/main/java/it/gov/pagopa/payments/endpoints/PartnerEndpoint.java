package it.gov.pagopa.payments.endpoints;

import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.partner.ObjectFactory;
import it.gov.pagopa.payments.model.partner.PaDemandPaymentNoticeRequest;
import it.gov.pagopa.payments.model.partner.PaDemandPaymentNoticeResponse;
import it.gov.pagopa.payments.model.partner.PaGetPaymentReq;
import it.gov.pagopa.payments.model.partner.PaGetPaymentRes;
import it.gov.pagopa.payments.model.partner.PaSendRTReq;
import it.gov.pagopa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeReq;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;
import it.gov.pagopa.payments.service.PartnerService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ws.server.endpoint.annotation.Endpoint;
import org.springframework.ws.server.endpoint.annotation.PayloadRoot;
import org.springframework.ws.server.endpoint.annotation.RequestPayload;
import org.springframework.ws.server.endpoint.annotation.ResponsePayload;
import org.springframework.ws.soap.server.endpoint.annotation.SoapAction;
import org.xml.sax.SAXException;

import javax.xml.bind.JAXBElement;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLStreamException;
import java.io.IOException;

@Endpoint
@Slf4j
public class PartnerEndpoint {

    @Autowired
    private PartnerService partnerService;

    @Autowired
    private ObjectFactory factory;

    @SoapAction("paVerifyPaymentNotice")
    @PayloadRoot(localPart = "paVerifyPaymentNoticeReq")
    @ResponsePayload
    public JAXBElement<PaVerifyPaymentNoticeRes> paVerifyPaymentNotice(
            @RequestPayload JAXBElement<PaVerifyPaymentNoticeReq> request)
            throws DatatypeConfigurationException, PartnerValidationException {

        log.info(" paVerifyPaymentNotice START ");
        return factory.createPaVerifyPaymentNoticeRes(partnerService.paVerifyPaymentNotice(request.getValue()));
    }

    @SoapAction("paGetPayment")
    @PayloadRoot(localPart = "paGetPaymentReq")
    @ResponsePayload
    public JAXBElement<PaGetPaymentRes> paGetPayment(@RequestPayload JAXBElement<PaGetPaymentReq> request)
            throws PartnerValidationException, DatatypeConfigurationException {

        log.info(" paGetPayment START ");
        return factory.createPaGetPaymentRes(partnerService.paGetPayment(request.getValue()));
    }

    @SoapAction("paSendRT")
    @PayloadRoot(localPart = "paSendRTReq")
    @ResponsePayload
    public JAXBElement<PaSendRTRes> paSendRT(@RequestPayload JAXBElement<PaSendRTReq> request) {

        log.info(" paSendRT START ");
        return factory.createPaSendRTRes(partnerService.paSendRT(request.getValue()));
    }

    @SoapAction("paDemandPaymentNotice")
    @PayloadRoot(localPart = "paDemandPaymentNotice")
    @ResponsePayload
    public JAXBElement<PaDemandPaymentNoticeResponse> paDemandPaymentNotice(@RequestPayload JAXBElement<PaDemandPaymentNoticeRequest> request)
            throws DatatypeConfigurationException, ParserConfigurationException, IOException, SAXException, XMLStreamException {

        log.info(" paDemandPaymentNotice START ");
        return factory.createPaDemandPaymentNoticeResponse(partnerService.paDemandPaymentNotice(request.getValue()));
    }
}
