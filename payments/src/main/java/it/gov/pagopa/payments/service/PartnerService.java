
package it.gov.pagopa.payments.service;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.TableBatchOperation;

import feign.FeignException;
import feign.RetryableException;
import it.gov.pagopa.payments.endpoints.validation.PaymentValidator;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.model.DebtPositionStatus;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import it.gov.pagopa.payments.model.PaymentOptionModel;
import it.gov.pagopa.payments.model.PaymentOptionModelResponse;
import it.gov.pagopa.payments.model.PaymentOptionStatus;
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
import it.gov.pagopa.payments.utils.AzuriteStorageUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class PartnerService {
	
	private String storageConnectionString = System.getenv("PAYMENTS_SA_CONNECTION_STRING");
    private String receiptsTable = System.getenv("RECEIPTS_TABLE");

    @Autowired
    private ObjectFactory factory;

    @Autowired
    private GpdClient gpdClient;

    @Autowired
    private PaymentValidator paymentValidator;
    
    public PartnerService() {}
    
    public PartnerService(ObjectFactory factory, String storageConnectionString, String receiptsTable, GpdClient gpdClient, PaymentValidator paymentValidator) {
        this.factory = factory;
    	this.storageConnectionString = storageConnectionString;
        this.receiptsTable = receiptsTable;
        this.gpdClient = gpdClient;
        this.paymentValidator = paymentValidator;
    }

    @Transactional(readOnly = true)
    public PaVerifyPaymentNoticeRes paVerifyPaymentNotice(PaVerifyPaymentNoticeReq request)
            throws DatatypeConfigurationException, PartnerValidationException {

        log.debug("[paVerifyPaymentNotice] isAuthorize check");
        paymentValidator.isAuthorize(request.getIdPA(), request.getIdBrokerPA(), request.getIdStation());

        log.debug("[paVerifyPaymentNotice] get payment option");
        PaymentsModelResponse paymentOption = null;

        try {
            // with Aux-Digit = 3
            // notice number format is define as follows:
            // 3<segregation code(2n)><IUV base(13n)><IUV check digit(2n)>
            // GPD service works on IUVs directly, so we remove the Aux-Digit
            paymentOption = gpdClient.getPaymentOption(request.getIdPA(), request.getQrCode().getNoticeNumber().substring(1));
        } catch (FeignException.NotFound e) {
            log.error("[paVerifyPaymentNotice] GPD Error not found", e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO);
        } catch (Exception e) {
            log.error("[paVerifyPaymentNotice] GPD Generic Error", e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_SYSTEM_ERROR);
        }

        checkDebtPositionStatus(paymentOption);

        log.info("[paVerifyPaymentNotice] Response OK generation");
        return this.generatePaVerifyPaymentNoticeResponse(paymentOption);
    }

    @Transactional(readOnly = true)
    public PaGetPaymentRes paGetPayment(PaGetPaymentReq request)
            throws DatatypeConfigurationException, PartnerValidationException {

        log.debug("[paGetPayment] isAuthorize check");
        paymentValidator.isAuthorize(request.getIdPA(), request.getIdBrokerPA(), request.getIdStation());

        log.debug("[paGetPayment] get payment option");
        PaymentsModelResponse paymentOption = null;

        try {
            // with Aux-Digit = 3
            // notice number format is define as follows:
            // 3<segregation code(2n)><IUV base(13n)><IUV check digit(2n)>
            // GPD service works on IUVs directly, so we remove the Aux-Digit
            paymentOption = gpdClient.getPaymentOption(request.getIdPA(), request.getQrCode().getNoticeNumber().substring(1));
        } catch (FeignException.NotFound e) {
            log.error("[paGetPayment] GPD Error not found", e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO);
        } catch (Exception e) {
            log.error("[paGetPayment] GPD Generic Error", e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_SYSTEM_ERROR);
        }

        checkDebtPositionStatus(paymentOption);

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
        PaymentOptionModelResponse paymentOption = null;
        try {
            // with Aux-Digit = 3
            // notice number format is define as follows:
            // 3<segregation code(2n)><IUV base(13n)><IUV check digit(2n)>
            // GPD service works on IUVs directly, so we remove the Aux-Digit
            paymentOption = gpdClient.receiptPaymentOption(request.getIdPA(), request.getReceipt().getNoticeNumber().substring(1), body);
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

        if (!paymentOption.getStatus().equals(PaymentOptionStatus.PO_PAID)) {
            log.error("[paSendRT] Payment Option status error: " + paymentOption.getStatus());
            throw new PartnerValidationException(PaaErrorEnum.PAA_SEMANTICA);
        }

        
        // save the receipt info 
        ReceiptEntity receiptEntity = new ReceiptEntity (request.getIdPA(), request.getReceipt().getCreditorReferenceId());
        String corporate = Optional.ofNullable(request.getReceipt().getDebtor())
        	.map(
        		CtSubject::getUniqueIdentifier
            ).map(
            	CtEntityUniqueIdentifier::getEntityUniqueIdentifierValue
            ).orElse("");
        receiptEntity.setDocument(this.marshal(request));
        receiptEntity.setCorporate(corporate);
        this.saveReceipt(receiptEntity);
        
        log.info("[paSendRT] Generate Response");
        // status is always equals to PO_PAID
        return generatePaSendRTResponse();
    }
    

    /**
     * Verify debt position status
     * @param paymentOption {@link PaymentsModelResponse} response from GPD
     */
    private void checkDebtPositionStatus(PaymentsModelResponse paymentOption) {
        if (paymentOption.getDebtPositionStatus().equals(DebtPositionStatus.EXPIRED)) {
            log.error("[Check DP] Debt position status error: " + paymentOption.getDebtPositionStatus());
            throw new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCADUTO);
        }
        else if (paymentOption.getDebtPositionStatus().equals(DebtPositionStatus.INVALID)) {
            log.error("[Check DP] Debt position status error: " + paymentOption.getDebtPositionStatus());
            throw new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_ANNULLATO);
        }
        else if (paymentOption.getDebtPositionStatus().equals(DebtPositionStatus.DRAFT) ||
                paymentOption.getDebtPositionStatus().equals(DebtPositionStatus.PUBLISHED)) {
            log.error("[Check DP] Debt position status error: " + paymentOption.getDebtPositionStatus());
            throw new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO);
        }
        else if (paymentOption.getDebtPositionStatus().equals(DebtPositionStatus.PARTIALLY_PAID) ||
                paymentOption.getDebtPositionStatus().equals(DebtPositionStatus.PAID) ||
                paymentOption.getDebtPositionStatus().equals(DebtPositionStatus.REPORTED)) {
            log.error("[Check DP] Debt position status error: " + paymentOption.getDebtPositionStatus());
            throw new PartnerValidationException(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO);
        }
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
        responseData.setCompanyName(Optional.ofNullable(source.getCompanyName()).orElse("NA"));
        responseData.setOfficeName(Optional.ofNullable(source.getOfficeName()).orElse(("NA")));

        // debitor data
        uniqueIdentifier.setEntityUniqueIdentifierType(StEntityUniqueIdentifierType.fromValue(source.getType().name()));

        uniqueIdentifier.setEntityUniqueIdentifierValue(source.getFiscalCode());

        debtor.setUniqueIdentifier(uniqueIdentifier);
        debtor.setFullName(source.getFullName());
        debtor.setStreetName(source.getStreetName());
        debtor.setCivicNumber(source.getCivicNumber());
        debtor.setPostalCode(source.getPostalCode());
        debtor.setCity(source.getCity());
        debtor.setStateProvinceRegion(source.getProvince());
        debtor.setCountry(source.getCountry());
        debtor.setEMail(source.getEmail());

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
        result.setCompanyName(Optional.ofNullable(source.getCompanyName()).orElse("NA"));
        result.setOfficeName(Optional.ofNullable(source.getOfficeName()).orElse(("NA")));
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


    private PaSendRTRes generatePaSendRTResponse() {
        PaSendRTRes result = factory.createPaSendRTRes();
        result.setOutcome(StOutcome.OK);
        return result;
    }
    
    private String marshal(PaSendRTReq paSendRTReq)   {
    	StringWriter sw = new StringWriter();
		try {
			JAXBContext context = JAXBContext.newInstance(PaSendRTReq.class);
			Marshaller mar= context.createMarshaller();
	        mar.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
	        JAXBElement<PaSendRTReq> jaxbElement = 
	                new JAXBElement<>( new QName("", "paSendRTReq"), 
	                		PaSendRTReq.class, 
	                		paSendRTReq);
	        mar.marshal(jaxbElement, sw);
		} catch (JAXBException e) {
			log.error("[paSendRT - marshal] error marshalling receipt: " + e.getMessage(), e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_SYSTEM_ERROR);
		}
        return sw.toString();
    }
    
    private void saveReceipt(ReceiptEntity receiptEntity)  {
        try {
        	AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString);
			azuriteStorageUtil.createTable(receiptsTable);
			CloudTable table = CloudStorageAccount.parse(storageConnectionString)
	                .createCloudTableClient()
	                .getTableReference(receiptsTable);
			TableBatchOperation batchOperation = new TableBatchOperation();

	        batchOperation.insert(receiptEntity);

	        table.execute(batchOperation);
		} catch (InvalidKeyException | URISyntaxException | StorageException | RuntimeException e) {
			log.error("[paSendRT - saveReceipt] error saving receipt: " + e.getMessage(), e);
            throw new PartnerValidationException(PaaErrorEnum.PAA_SYSTEM_ERROR);
		}     
    }
    
    

}
