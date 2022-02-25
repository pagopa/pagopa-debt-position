
package it.gov.pagopa.payments.service;

import it.gov.pagopa.payments.endpoints.validation.PaymentValidator;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.model.PaaErrorEnum;
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


    //    @Transactional
//    public PaSendRTRes paSendRT(PaSendRTReq request) {
//
//        log.debug(String.format("[paSendRT] get Payment %s", request.getReceipt().getNoticeNumber()));
//        PaymentOptions option = paymentOptionsRepository
//                .findByNotificationCodeAndFiscalCode(request.getReceipt().getNoticeNumber(),
//                        request.getReceipt().getFiscalCode())
//                .orElseThrow(() -> new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO));
//
//        log.debug("[paSendRT] isAuthorize check");
//        paymentValidator.isAuthorize(request.getIdPA(), request.getIdBrokerPA(), request.getIdStation(),
//                option.getFiscalCode());
//
//        log.debug("[paSendRT] isPayable check");
//        paymentValidator.isPayable(option.getPaymentPosition(), option);
//
//        log.debug("[paSendRT] Update Status Option to PAGATO");
//        option.setStatus(PaymentOptionStatusEnum.PAGATO.getStatus());
//
//        log.debug("[paSendRT] Update Paid Option in Position");
//        Integer paidOptionUpdated = option.getPaymentPosition().getPaidOptions() + 1;
//        Integer totalOption = option.getPaymentPosition().getTotalOptions();
//        option.getPaymentPosition().setPaidOptions(paidOptionUpdated);
//
//        log.debug("[paSendRT] Insert receipt data");
//        option.setFee(request.getReceipt().getFee());
//        option.setPspCompanyName(request.getReceipt().getPSPCompanyName());
//        option.setPaymentMethod(request.getReceipt().getPaymentMethod());
//        option.setReceiptId(request.getReceipt().getReceiptId());
//        option.setPaymentDate(
//                request.getReceipt().getPaymentDateTime().toGregorianCalendar().toZonedDateTime().toLocalDateTime());
//        option.setReceipt(this.getReceiptFromRequest(request));
//        /**
//         * Position Status is updated to PAGATO if one of the two condiction is true:
//         *
//         * 1. option is the only refered to the position (isConclusive == true);
//         *
//         * 2. option is the last refered to the position (paidOptionUpdated ==
//         * totalOption - 1).
//         */
//        log.debug("[paSendRT] Update Position Status");
//        Integer positionStatus = Boolean.TRUE.equals(option.getIsConclusive())
//                || paidOptionUpdated.compareTo(totalOption - 1) == 0 ? PaymentStatusEnum.PAGATO.getStatus()
//                : PaymentStatusEnum.PAGATO_PARZIALE.getStatus();
//        option.getPaymentPosition().setStatus(positionStatus);
//        paymentOptionsRepository.save(option);
//
//        log.info("[paSendRT] Generate Response");
//        return this.generatePaSendRTResponse(StOutcome.OK);
//    }
//
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


//    private PaGetPaymentRes generatePaGetPaymentResponse(PaymentPosition position, PaymentOptions option,
//                                                         String creditorReferenceId, StTransferType transferType) throws DatatypeConfigurationException {
//        PaGetPaymentRes response = factory.createPaGetPaymentRes();
//        CtPaymentPA responseData = factory.createCtPaymentPA();
//        CtSubject debitor = factory.createCtSubject();
//        CtEntityUniqueIdentifier uniqueIdentifier = factory.createCtEntityUniqueIdentifier();
//        CtTransferListPA transferList = factory.createCtTransferListPA();
//
//        response.setOutcome(StOutcome.OK);
//
//        // general payment data
//        responseData.setCreditorReferenceId(creditorReferenceId);
//        responseData.setPaymentAmount(option.getAmount());
//        responseData.setDueDate(DatatypeFactory.newInstance().newXMLGregorianCalendar(option.getDuoDate().toString()));
//        responseData.setRetentionDate(option.getRetentionDate() != null
//                ? DatatypeFactory.newInstance().newXMLGregorianCalendar(option.getRetentionDate().toString())
//                : null);
//        responseData.setLastPayment(false); // de-scoping
//        responseData.setDescription(position.getDescription());
//        responseData.setCompanyName(Optional.ofNullable(position.getCompanyName()).orElse("NA"));
//        responseData.setOfficeName(Optional.ofNullable(position.getOfficeName()).orElse(("NA")));
//
//        // debitor data
//        uniqueIdentifier.setEntityUniqueIdentifierType(
//                position.getDebitor().getType().equals(1) ? StEntityUniqueIdentifierType.F
//                        : StEntityUniqueIdentifierType.G);
//        uniqueIdentifier.setEntityUniqueIdentifierValue(position.getDebitor().getFiscalCode());
//        debitor.setUniqueIdentifier(uniqueIdentifier);
//        debitor.setFullName(position.getDebitor().getName());
//        debitor.setStreetName(position.getDebitor().getAddress());
//        debitor.setCivicNumber(position.getDebitor().getNumber());
//        debitor.setPostalCode(position.getDebitor().getCap());
//        debitor.setCity(position.getDebitor().getArea());
//        debitor.setStateProvinceRegion(position.getDebitor().getProvince());
//        debitor.setCountry(position.getDebitor().getCountry());
//        debitor.setEMail(position.getDebitor().getEmail());
//
//        // Transfer list
//        transferList.getTransfer()
//                .addAll(IntStream.range(0, option.getTransfers().size())
//                        .mapToObj(index -> getTransferResponse(option.getTransfers().get(index), index, transferType))
//                        .collect(Collectors.toList()));
//
//        responseData.setTransferList(transferList);
//        responseData.setDebtor(debitor);
//        response.setData(responseData);
//
//        return response;
//    }
//
//    private PaSendRTRes generatePaSendRTResponse(StOutcome ok) {
//
//        PaSendRTRes result = factory.createPaSendRTRes();
//        result.setOutcome(ok);
//        return result;
//    }
//


//
//    private String getReceiptFromRequest(PaSendRTReq request) {
//
//        StringWriter sw = new StringWriter();
//        JAXB.marshal(request, sw);
//        return sw.toString();
//    }
}
