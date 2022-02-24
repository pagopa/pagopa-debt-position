
package it.gov.pagopa.hubpa.payments.service;

import java.io.StringWriter;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.xml.bind.JAXB;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import it.gov.pagopa.hubpa.payments.endpoints.validation.PaymentValidator;
import it.gov.pagopa.hubpa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.hubpa.payments.entity.PaymentOptions;
import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;
import it.gov.pagopa.hubpa.payments.entity.Transfers;
import it.gov.pagopa.hubpa.payments.enumeration.PaymentOptionStatusEnum;
import it.gov.pagopa.hubpa.payments.enumeration.PaymentStatusEnum;
import it.gov.pagopa.hubpa.payments.model.PaaErrorEnum;
import it.gov.pagopa.hubpa.payments.model.partner.CtEntityUniqueIdentifier;
import it.gov.pagopa.hubpa.payments.model.partner.CtPaymentOptionDescriptionPA;
import it.gov.pagopa.hubpa.payments.model.partner.CtPaymentOptionsDescriptionListPA;
import it.gov.pagopa.hubpa.payments.model.partner.CtPaymentPA;
import it.gov.pagopa.hubpa.payments.model.partner.CtSubject;
import it.gov.pagopa.hubpa.payments.model.partner.CtTransferListPA;
import it.gov.pagopa.hubpa.payments.model.partner.CtTransferPA;
import it.gov.pagopa.hubpa.payments.model.partner.ObjectFactory;
import it.gov.pagopa.hubpa.payments.model.partner.PaGetPaymentReq;
import it.gov.pagopa.hubpa.payments.model.partner.PaGetPaymentRes;
import it.gov.pagopa.hubpa.payments.model.partner.PaSendRTReq;
import it.gov.pagopa.hubpa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.hubpa.payments.model.partner.PaVerifyPaymentNoticeReq;
import it.gov.pagopa.hubpa.payments.model.partner.PaVerifyPaymentNoticeRes;
import it.gov.pagopa.hubpa.payments.model.partner.StAmountOption;
import it.gov.pagopa.hubpa.payments.model.partner.StEntityUniqueIdentifierType;
import it.gov.pagopa.hubpa.payments.model.partner.StOutcome;
import it.gov.pagopa.hubpa.payments.model.partner.StTransferType;
import it.gov.pagopa.hubpa.payments.repository.IncrementalIuvNumberRepository;
import it.gov.pagopa.hubpa.payments.repository.PaymentOptionsRepository;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PartnerService {

    @Autowired
    IncrementalIuvNumberRepository incrementalIuvNumberRepository;

    @Autowired
    private PaymentOptionsRepository paymentOptionsRepository;

    @Autowired
    private ObjectFactory factory;

    @Autowired
    private PaymentValidator paymentValidator;

    @Transactional(readOnly = true)
    public PaVerifyPaymentNoticeRes paVerifyPaymentNotice(PaVerifyPaymentNoticeReq request)
            throws DatatypeConfigurationException, PartnerValidationException {

        log.debug(String.format("[paVerifyPaymentNotice] get Payment %s", request.getQrCode().getNoticeNumber()));
        PaymentOptions option = paymentOptionsRepository
                .findByNotificationCodeAndFiscalCode(request.getQrCode().getNoticeNumber(),
                        request.getQrCode().getFiscalCode())
                .orElseThrow(() -> new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO));

        log.debug("[paVerifyPaymentNotice] isAuthorize check");
        paymentValidator.isAuthorize(request.getIdPA(), request.getIdBrokerPA(), request.getIdStation(),
                option.getFiscalCode());

        log.debug("[paVerifyPaymentNotice] isPayable check");
        paymentValidator.isPayable(option.getPaymentPosition(), option);

        log.info("[paVerifyPaymentNotice] Response OK generation");
        return this.generatePaVerifyPaymentNoticeResponse(option.getPaymentPosition(), option, request.getIdPA());
    }

    @Transactional(readOnly = true)
    public PaGetPaymentRes paGetPayment(PaGetPaymentReq request)
            throws DatatypeConfigurationException, PartnerValidationException {

        log.debug(String.format("[paGetPayment] get Payment %s", request.getQrCode().getNoticeNumber()));
        PaymentOptions option = paymentOptionsRepository
                .findByNotificationCodeAndFiscalCode(request.getQrCode().getNoticeNumber(),
                        request.getQrCode().getFiscalCode())
                .orElseThrow(() -> new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO));

        log.debug("[paGetPayment] isAuthorize check");
        paymentValidator.isAuthorize(request.getIdPA(), request.getIdBrokerPA(), request.getIdStation(),
                option.getFiscalCode());

        log.debug("[paGetPayment] isPayable check");
        paymentValidator.isPayable(option.getPaymentPosition(), option);

        log.info("[paGetPayment] Response OK generation");
        return this.generatePaGetPaymentResponse(option.getPaymentPosition(), option,
                request.getQrCode().getNoticeNumber().substring(1), request.getTransferType());
    }

    @Transactional
    public PaSendRTRes paSendRT(PaSendRTReq request) {

        log.debug(String.format("[paSendRT] get Payment %s", request.getReceipt().getNoticeNumber()));
        PaymentOptions option = paymentOptionsRepository
                .findByNotificationCodeAndFiscalCode(request.getReceipt().getNoticeNumber(),
                        request.getReceipt().getFiscalCode())
                .orElseThrow(() -> new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO));

        log.debug("[paSendRT] isAuthorize check");
        paymentValidator.isAuthorize(request.getIdPA(), request.getIdBrokerPA(), request.getIdStation(),
                option.getFiscalCode());

        log.debug("[paSendRT] isPayable check");
        paymentValidator.isPayable(option.getPaymentPosition(), option);

        log.debug("[paSendRT] Update Status Option to PAGATO");
        option.setStatus(PaymentOptionStatusEnum.PAGATO.getStatus());

        log.debug("[paSendRT] Update Paid Option in Position");
        Integer paidOptionUpdated = option.getPaymentPosition().getPaidOptions() + 1;
        Integer totalOption = option.getPaymentPosition().getTotalOptions();
        option.getPaymentPosition().setPaidOptions(paidOptionUpdated);

        log.debug("[paSendRT] Insert receipt data");
        option.setFee(request.getReceipt().getFee());
        option.setPspCompanyName(request.getReceipt().getPSPCompanyName());
        option.setPaymentMethod(request.getReceipt().getPaymentMethod());
        option.setReceiptId(request.getReceipt().getReceiptId());
        option.setPaymentDate(
                request.getReceipt().getPaymentDateTime().toGregorianCalendar().toZonedDateTime().toLocalDateTime());
        option.setReceipt(this.getReceiptFromRequest(request));
        /**
         * Position Status is updated to PAGATO if one of the two condiction is true:
         * 
         * 1. option is the only refered to the position (isConclusive == true);
         * 
         * 2. option is the last refered to the position (paidOptionUpdated ==
         * totalOption - 1).
         */
        log.debug("[paSendRT] Update Position Status");
        Integer positionStatus = Boolean.TRUE.equals(option.getIsConclusive())
                || paidOptionUpdated.compareTo(totalOption - 1) == 0 ? PaymentStatusEnum.PAGATO.getStatus()
                        : PaymentStatusEnum.PAGATO_PARZIALE.getStatus();
        option.getPaymentPosition().setStatus(positionStatus);
        paymentOptionsRepository.save(option);

        log.info("[paSendRT] Generate Response");
        return this.generatePaSendRTResponse(StOutcome.OK);
    }

    private CtTransferPA getTransferResponse(Transfers transfer, int counter, StTransferType transferType) {

        CtTransferPA transferPa = factory.createCtTransferPA();
        transferPa.setFiscalCodePA(transfer.getOrganizationFiscalCode());
        transferPa.setIBAN(this.getIbanByTransferType(transferType, transfer));
        transferPa.setIdTransfer(counter + 1);
        transferPa.setRemittanceInformation(transfer.getReason());
        transferPa.setTransferAmount(transfer.getPartialAmount());
        transferPa.setTransferCategory(transfer.getTaxonomy().replace("/", "").substring(1));
        return transferPa;
    }

    private PaVerifyPaymentNoticeRes generatePaVerifyPaymentNoticeResponse(PaymentPosition position,
            PaymentOptions option, String fiscalCodePa) throws DatatypeConfigurationException {

        PaVerifyPaymentNoticeRes result = factory.createPaVerifyPaymentNoticeRes();
        CtPaymentOptionsDescriptionListPA paymentList = factory.createCtPaymentOptionsDescriptionListPA();
        CtPaymentOptionDescriptionPA paymentOption = factory.createCtPaymentOptionDescriptionPA();
        // generare una paVerifyPaymentNoticeRes positiva
        result.setOutcome(StOutcome.OK);
        // paymentList
        paymentOption.setAmount(option.getAmount());
        paymentOption.setOptions(StAmountOption.EQ); // de-scoping
        paymentOption.setDueDate(DatatypeFactory.newInstance().newXMLGregorianCalendar(option.getDuoDate().toString()));
        paymentOption.setDetailDescription(position.getDescription());
        paymentOption.setAllCCP(option.getAllCpp()); // allCPP fa parte del modello del option
        paymentList.getPaymentOptionDescription().add(paymentOption);

        result.setPaymentList(paymentList);
        // general info
        result.setPaymentDescription(position.getDescription());
        result.setFiscalCodePA(fiscalCodePa);
        result.setCompanyName(Optional.ofNullable(position.getCompanyName()).orElse("NA"));
        result.setOfficeName(Optional.ofNullable(position.getOfficeName()).orElse(("NA")));
        return result;
    }

    private PaGetPaymentRes generatePaGetPaymentResponse(PaymentPosition position, PaymentOptions option,
            String creditorReferenceId, StTransferType transferType) throws DatatypeConfigurationException {
        PaGetPaymentRes response = factory.createPaGetPaymentRes();
        CtPaymentPA responseData = factory.createCtPaymentPA();
        CtSubject debitor = factory.createCtSubject();
        CtEntityUniqueIdentifier uniqueIdentifier = factory.createCtEntityUniqueIdentifier();
        CtTransferListPA transferList = factory.createCtTransferListPA();

        response.setOutcome(StOutcome.OK);

        // general payment data
        responseData.setCreditorReferenceId(creditorReferenceId);
        responseData.setPaymentAmount(option.getAmount());
        responseData.setDueDate(DatatypeFactory.newInstance().newXMLGregorianCalendar(option.getDuoDate().toString()));
        responseData.setRetentionDate(option.getRetentionDate() != null
                ? DatatypeFactory.newInstance().newXMLGregorianCalendar(option.getRetentionDate().toString())
                : null);
        responseData.setLastPayment(false); // de-scoping
        responseData.setDescription(position.getDescription());
        responseData.setCompanyName(Optional.ofNullable(position.getCompanyName()).orElse("NA"));
        responseData.setOfficeName(Optional.ofNullable(position.getOfficeName()).orElse(("NA")));

        // debitor data
        uniqueIdentifier.setEntityUniqueIdentifierType(
                position.getDebitor().getType().equals(1) ? StEntityUniqueIdentifierType.F
                        : StEntityUniqueIdentifierType.G);
        uniqueIdentifier.setEntityUniqueIdentifierValue(position.getDebitor().getFiscalCode());
        debitor.setUniqueIdentifier(uniqueIdentifier);
        debitor.setFullName(position.getDebitor().getName());
        debitor.setStreetName(position.getDebitor().getAddress());
        debitor.setCivicNumber(position.getDebitor().getNumber());
        debitor.setPostalCode(position.getDebitor().getCap());
        debitor.setCity(position.getDebitor().getArea());
        debitor.setStateProvinceRegion(position.getDebitor().getProvince());
        debitor.setCountry(position.getDebitor().getCountry());
        debitor.setEMail(position.getDebitor().getEmail());

        // Transfer list
        transferList.getTransfer()
                .addAll(IntStream.range(0, option.getTransfers().size())
                        .mapToObj(index -> getTransferResponse(option.getTransfers().get(index), index, transferType))
                        .collect(Collectors.toList()));

        responseData.setTransferList(transferList);
        responseData.setDebtor(debitor);
        response.setData(responseData);

        return response;
    }

    private PaSendRTRes generatePaSendRTResponse(StOutcome ok) {

        PaSendRTRes result = factory.createPaSendRTRes();
        result.setOutcome(ok);
        return result;
    }

    /**
     * The method return iban given transferType and transfer, according to
     * https://pagopa.atlassian.net/wiki/spaces/PAG/pages/96403906/paGetPayment#trasferType
     */
    private String getIbanByTransferType(StTransferType transferType, Transfers transfer) {

        String dafaultIban = Optional.ofNullable(transfer.getIban())
                .orElseGet(() -> Optional.ofNullable(transfer.getPostalIban())
                        .orElseThrow(() -> new PartnerValidationException(PaaErrorEnum.PAA_SEMANTICA)));

        return transferType != null && transferType.value().equals(StTransferType.POSTAL.value())
                && transfer.getPostalIban() != null ? transfer.getPostalIban() : dafaultIban;
    }


    private String getReceiptFromRequest(PaSendRTReq request) {

        StringWriter sw = new StringWriter();
        JAXB.marshal(request, sw);
        return sw.toString();
    }
}
