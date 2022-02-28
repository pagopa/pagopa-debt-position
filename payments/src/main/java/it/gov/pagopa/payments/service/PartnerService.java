
package it.gov.pagopa.payments.service;

import feign.FeignException;
import feign.RetryableException;
import it.gov.pagopa.payments.endpoints.validation.PaymentValidator;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import it.gov.pagopa.payments.model.PaymentOptionModel;
import it.gov.pagopa.payments.model.PaymentsModelResponse;
import it.gov.pagopa.payments.model.PaymentsTransferModelResponse;
import it.gov.pagopa.payments.model.partner.CtEntityUniqueIdentifier;
import it.gov.pagopa.payments.model.partner.CtPaymentOptionDescriptionPA;
import it.gov.pagopa.payments.model.partner.CtPaymentOptionsDescriptionListPA;
import it.gov.pagopa.payments.model.partner.CtPaymentPA;
import it.gov.pagopa.payments.model.partner.CtSubject;
import it.gov.pagopa.payments.model.partner.CtTransferListPA;
import it.gov.pagopa.payments.model.partner.CtTransferPA;
import it.gov.pagopa.payments.model.partner.ObjectFactory;
import it.gov.pagopa.payments.model.partner.PaGetPaymentReq;
import it.gov.pagopa.payments.model.partner.PaGetPaymentRes;
import it.gov.pagopa.payments.model.partner.PaSendRTReq;
import it.gov.pagopa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeReq;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;
import it.gov.pagopa.payments.model.partner.StAmountOption;
import it.gov.pagopa.payments.model.partner.StEntityUniqueIdentifierType;
import it.gov.pagopa.payments.model.partner.StOutcome;
import it.gov.pagopa.payments.model.partner.StTransferType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import java.math.BigDecimal;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Slf4j
public class PartnerService {

    @Autowired
    private ObjectFactory factory;

    @Autowired
    private GpdClient gpdClient;

    @Autowired
    private PaymentValidator paymentValidator;

    @Transactional(readOnly = true)
    public PaVerifyPaymentNoticeRes paVerifyPaymentNotice(PaVerifyPaymentNoticeReq request)
            throws DatatypeConfigurationException, PartnerValidationException {

        log.debug("[paVerifyPaymentNotice] isAuthorize check");
        paymentValidator.isAuthorize(request.getIdPA(), request.getIdBrokerPA(), request.getIdStation());

        log.debug("[paVerifyPaymentNotice] get payment option");
        var paymentOption = gpdClient.getPaymentOption(request.getIdPA(), request.getQrCode().getNoticeNumber());

        log.info("[paVerifyPaymentNotice] Response OK generation");
        return this.generatePaVerifyPaymentNoticeResponse(paymentOption);
    }

    @Transactional(readOnly = true)
    public PaGetPaymentRes paGetPayment(PaGetPaymentReq request)
            throws DatatypeConfigurationException, PartnerValidationException {

        log.debug("[paGetPayment] isAuthorize check");
        paymentValidator.isAuthorize(request.getIdPA(), request.getIdBrokerPA(), request.getIdStation());

        log.debug("[paGetPayment] get payment option");
        var paymentOption = gpdClient.getPaymentOption(request.getIdPA(), request.getQrCode().getNoticeNumber());

        log.info("[paGetPayment] Response OK generation");
        return this.generatePaGetPaymentResponse(paymentOption, request);
    }

    @Transactional
    public PaSendRTRes paSendRT(PaSendRTReq request) {

        log.debug("[paSendRT] isAuthorize check");
        paymentValidator.isAuthorize(request.getIdPA(), request.getIdBrokerPA(), request.getIdStation());

        log.debug(String.format("[paSendRT] get receipt payment option %s", request.getReceipt().getNoticeNumber()));
        PaymentOptionModel body = PaymentOptionModel.builder()
                .idReceipt(request.getReceipt().getReceiptId())
                .paymentDate(request.getReceipt().getPaymentDateTime().toGregorianCalendar().toZonedDateTime().toLocalDateTime())
                .pspCompany(request.getReceipt().getPSPCompanyName())
                .paymentMethod(request.getReceipt().getPaymentMethod())
                .build();

        try {
            gpdClient.receiptPaymentOption(request.getIdPA(), request.getReceipt().getNoticeNumber(), body);
        } catch (FeignException.Conflict e) {
            log.error("[paSendRT] GPD Conflict Error Response", e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO);
        } catch (RetryableException e) {
            log.error("[paSendRT] GPD Not Reachable", e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_SYSTEM_ERROR);
        } catch (FeignException e) {
            log.error("[paSendRT] GPD Error Response", e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_SEMANTICA);
        } catch (Exception e) {
            log.error("[paSendRT] GPD Generic Error", e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_SYSTEM_ERROR);
        }

        log.info("[paSendRT] Generate Response");
        // status is always equals to PO_PAID
        return generatePaSendRTResponse(StOutcome.OK);
    }


    /**
     * map the response of GPD in the XML model
     *
     * @param source  {@link PaymentsModelResponse} response from GPD
     * @param request SOAP input model
     * @return XML model
     * @throws DatatypeConfigurationException If the DatatypeFactory is not available or cannot be instantiated.
     */
    private PaGetPaymentRes generatePaGetPaymentResponse(PaymentsModelResponse source, PaGetPaymentReq request)
            throws DatatypeConfigurationException {

        PaGetPaymentRes response = factory.createPaGetPaymentRes();
        CtPaymentPA responseData = factory.createCtPaymentPA();
        CtSubject debtor = factory.createCtSubject();
        CtEntityUniqueIdentifier uniqueIdentifier = factory.createCtEntityUniqueIdentifier();
        CtTransferListPA transferList = factory.createCtTransferListPA();

        response.setOutcome(StOutcome.OK);

        // general payment data
        responseData.setCreditorReferenceId(source.getOrganizationFiscalCode());
        responseData.setPaymentAmount(BigDecimal.valueOf(source.getAmount()));
        responseData.setDueDate(DatatypeFactory.newInstance().newXMLGregorianCalendar(source.getDueDate().toString()));
        responseData.setRetentionDate(source.getRetentionDate() != null
                ? DatatypeFactory.newInstance().newXMLGregorianCalendar(source.getRetentionDate().toString())
                : null);
        responseData.setLastPayment(false); // de-scoping
        responseData.setDescription(source.getDescription());
//        responseData.setCompanyName(Optional.ofNullable(source.getCompanyName()).orElse("NA")); TODO
//        responseData.setOfficeName(Optional.ofNullable(source.getOfficeName()).orElse(("NA"))); TODO

        // debitor data
//        uniqueIdentifier.setEntityUniqueIdentifierType(
//                source.getDebitor().getType().equals(1) ? StEntityUniqueIdentifierType.F TODO
//                        : StEntityUniqueIdentifierType.G);
        uniqueIdentifier.setEntityUniqueIdentifierType(StEntityUniqueIdentifierType.G);

//        uniqueIdentifier.setEntityUniqueIdentifierValue(source.getDebitor().getFiscalCode()); TODO
        uniqueIdentifier.setEntityUniqueIdentifierValue("ABC");

        debtor.setUniqueIdentifier(uniqueIdentifier);
//        debtor.setFullName(position.getDebitor().getName());  TODO
//        debtor.setStreetName(position.getDebitor().getAddress()); TODO
//        debtor.setCivicNumber(position.getDebitor().getNumber()); TODO
//        debtor.setPostalCode(position.getDebitor().getCap()); TODO
//        debtor.setCity(position.getDebitor().getArea()); TODO
//        debtor.setStateProvinceRegion(position.getDebitor().getProvince()); TODO
//        debtor.setCountry(position.getDebitor().getCountry()); TODO
//        debtor.setEMail(position.getDebitor().getEmail()); TODO

        // Transfer list
        transferList.getTransfer()
                .addAll(source.getTransfer()
                        .stream()
                        .map(paymentsTransferModelResponse -> getTransferResponse(paymentsTransferModelResponse, request.getTransferType()))
                        .collect(Collectors.toList()));

        responseData.setTransferList(transferList);
        responseData.setDebtor(debtor);
        response.setData(responseData);

        return response;
    }


    /**
     * map the response of GPD in the XML model
     *
     * @param source {@link PaymentsModelResponse} response from GPD
     * @return XML model
     * @throws DatatypeConfigurationException If the DatatypeFactory is not available or cannot be instantiated.
     */
    private PaVerifyPaymentNoticeRes generatePaVerifyPaymentNoticeResponse(PaymentsModelResponse source)
            throws DatatypeConfigurationException {

        PaVerifyPaymentNoticeRes result = factory.createPaVerifyPaymentNoticeRes();
        CtPaymentOptionsDescriptionListPA paymentList = factory.createCtPaymentOptionsDescriptionListPA();
        CtPaymentOptionDescriptionPA paymentOption = factory.createCtPaymentOptionDescriptionPA();
        // generare una paVerifyPaymentNoticeRes positiva
        result.setOutcome(StOutcome.OK);
        // paymentList
        paymentOption.setAmount(BigDecimal.valueOf(source.getAmount()));
        paymentOption.setOptions(StAmountOption.EQ); // de-scoping
        paymentOption.setDueDate(DatatypeFactory.newInstance().newXMLGregorianCalendar(source.getDueDate().toString()));
        paymentOption.setDetailDescription(source.getDescription());
        var cpp = source.getTransfer().stream()
                .noneMatch(elem -> elem.getPostalIban() == null || elem.getPostalIban().isBlank());
        paymentOption.setAllCCP(cpp); // allCPP fa parte del modello del option
        paymentList.getPaymentOptionDescription().add(paymentOption);

        result.setPaymentList(paymentList);
        // general info
        result.setPaymentDescription(source.getDescription());
        result.setFiscalCodePA(source.getOrganizationFiscalCode());
//        result.setCompanyName(Optional.ofNullable(source.getCompanyName()).orElse("NA")); TODO
//        result.setOfficeName(Optional.ofNullable(position.getOfficeName()).orElse(("NA"))); TODO
        return result;
    }


    /**
     * @param transfer     GPD response
     * @param transferType XML request
     * @return maps input into {@link CtTransferPA} model
     */
    private CtTransferPA getTransferResponse(PaymentsTransferModelResponse transfer, StTransferType transferType) {
        CtTransferPA transferPa = factory.createCtTransferPA();
        transferPa.setFiscalCodePA(transfer.getOrganizationFiscalCode());
        transferPa.setIBAN(getIbanByTransferType(transferType, transfer));
        transferPa.setIdTransfer(Integer.parseInt(transfer.getIdTransfer()));
        transferPa.setRemittanceInformation(transfer.getRemittanceInformation());
        transferPa.setTransferAmount(BigDecimal.valueOf(transfer.getAmount()));
        transferPa.setTransferCategory(transfer.getCategory().replace("/", "").substring(1));
        return transferPa;
    }


    /**
     * The method return iban given transferType and transfer, according to
     * https://pagopa.atlassian.net/wiki/spaces/PAG/pages/96403906/paGetPayment#trasferType
     */
    private String getIbanByTransferType(StTransferType transferType, PaymentsTransferModelResponse transfer) {

        String defaultIban = Optional.ofNullable(transfer.getIban())
                .orElseGet(() -> Optional.ofNullable(transfer.getPostalIban())
                        .orElseThrow(() -> new PartnerValidationException(PaaErrorEnum.PAA_SEMANTICA)));

        return transferType != null && transferType.value().equals(StTransferType.POSTAL.value())
                && transfer.getPostalIban() != null ? transfer.getPostalIban() : defaultIban;
    }


    private PaSendRTRes generatePaSendRTResponse(StOutcome outcome) {
        PaSendRTRes result = factory.createPaSendRTRes();
        result.setOutcome(outcome);
        return result;
    }

}
