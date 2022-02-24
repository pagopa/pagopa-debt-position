package it.gov.pagopa.payments.endpoints;

import javax.xml.bind.JAXBElement;
import javax.xml.datatype.DatatypeConfigurationException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ws.server.endpoint.annotation.Endpoint;
import org.springframework.ws.server.endpoint.annotation.PayloadRoot;
import org.springframework.ws.server.endpoint.annotation.RequestPayload;
import org.springframework.ws.server.endpoint.annotation.ResponsePayload;
import org.springframework.ws.soap.server.endpoint.annotation.SoapAction;

import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.partner.ObjectFactory;
import it.gov.pagopa.payments.model.partner.PaGetPaymentReq;
import it.gov.pagopa.payments.model.partner.PaGetPaymentRes;
import it.gov.pagopa.payments.model.partner.PaSendRTReq;
import it.gov.pagopa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeReq;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;
import it.gov.pagopa.payments.service.PartnerService;
import lombok.extern.slf4j.Slf4j;

@Endpoint
@Slf4j
public class PartnerEndpoint {

  @Autowired
  private PartnerService partnerService;

  @Autowired
  private ObjectFactory factory;

  @SoapAction("paVerifyPaymentNotice")
  @PayloadRoot(localPart = "paVerifyPaymentNoticeReq", namespace = "http://pagopa-api.pagopa.gov.it/partner")
  @ResponsePayload
  public JAXBElement<PaVerifyPaymentNoticeRes> paVerifyPaymentNotice(
      @RequestPayload JAXBElement<PaVerifyPaymentNoticeReq> request)
      throws DatatypeConfigurationException, PartnerValidationException {

    log.info(" paVerifyPaymentNotice START ");
    return factory.createPaVerifyPaymentNoticeRes(partnerService.paVerifyPaymentNotice(request.getValue()));
  }

  @SoapAction("paGetPayment")
  @PayloadRoot(localPart = "paGetPaymentReq", namespace = "http://pagopa-api.pagopa.gov.it/partner")
  @ResponsePayload
  public JAXBElement<PaGetPaymentRes> paGetPayment(@RequestPayload JAXBElement<PaGetPaymentReq> request)
      throws PartnerValidationException, DatatypeConfigurationException {

    log.info(" paGetPayment START ");
    return factory.createPaGetPaymentRes(partnerService.paGetPayment(request.getValue()));
  }

  @SoapAction("paSendRT")
  @PayloadRoot(localPart = "paSendRTReq", namespace = "http://pagopa-api.pagopa.gov.it/partner")
  @ResponsePayload
  public JAXBElement<PaSendRTRes> paSendRT(@RequestPayload JAXBElement<PaSendRTReq> request) {

    log.info(" paSendRT START ");
    return factory.createPaSendRTRes(partnerService.paSendRT(request.getValue()));
  }
}
