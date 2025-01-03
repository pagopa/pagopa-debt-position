package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.response.TransferMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.model.v3.response.InstallmentMetadataModelResponse;
import it.gov.pagopa.debtposition.model.v3.response.InstallmentModelResponse;
import it.gov.pagopa.debtposition.model.v3.response.PaymentOptionModelResponseV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;
import org.springframework.util.CollectionUtils;

import java.util.Collections;
import java.util.List;

public class ConvertPPEntityToPPv3Response implements Converter<PaymentPosition, PaymentPositionModelResponseV3> {
    @Override
    public PaymentPositionModelResponseV3 convert(MappingContext<PaymentPosition, PaymentPositionModelResponseV3> context) {
        PaymentPosition source = context.getSource();
        PaymentPositionModelResponseV3 destination = new PaymentPositionModelResponseV3();

        destination.setIupd(source.getIupd());
        destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setInsertedDate(source.getInsertedDate());
        destination.setPublishDate(source.getPublishDate());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setLastUpdatedDate(source.getLastUpdatedDate());
        // todo check for status coherence
        destination.setStatus(DebtPositionStatusV3.valueOf(source.getStatus().name()));

        // Partial payment PO
        List<PaymentOption> partialPO = source.getPaymentOption()
                .stream()
                .filter(po -> Boolean.TRUE.equals(po.getIsPartialPayment()))
                .toList();
        if(!partialPO.isEmpty()) {
            // get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
            PaymentOptionModelResponseV3 pov3 = convert(partialPO.get(0));
            pov3.setValidityDate(source.getValidityDate());
            pov3.setSwitchToExpired(source.getSwitchToExpired());
            // set installments
            List<InstallmentModelResponse> installments = partialPO.stream()
                    .map(this::convertInstallment)
                    .toList();
            pov3.setInstallments(installments);
            destination.addPaymentOption(pov3);
        }

        // Unique payment PO
        List<PaymentOption> uniquePO = source.getPaymentOption()
                .stream()
                .filter(po -> Boolean.FALSE.equals(po.getIsPartialPayment()))
                .toList();
        if(!uniquePO.isEmpty()) {
            PaymentOptionModelResponseV3 pov3;
            for (PaymentOption po: uniquePO) {
                pov3 = convert(po);
                pov3.setValidityDate(source.getValidityDate());
                pov3.setSwitchToExpired(source.getSwitchToExpired());
                // set installment
                List<InstallmentModelResponse> installments = Collections.singletonList(convertInstallment(po));
                pov3.setInstallments(installments);
                destination.addPaymentOption(pov3);
            }
        }

        return destination;
    }

    private InstallmentModelResponse convertInstallment(PaymentOption po) {
        InstallmentModelResponse inst = new InstallmentModelResponse();

        inst.setNav(po.getNav());
        inst.setIuv(po.getIuv());
        inst.setOrganizationFiscalCode(po.getOrganizationFiscalCode());
        inst.setAmount(po.getAmount());
        inst.setDescription(po.getDescription());
        inst.setDueDate(po.getDueDate());
        inst.setPaymentDate(po.getPaymentDate());
        inst.setReportingDate(po.getReportingDate());
        inst.setPaymentMethod(po.getPaymentMethod());
        inst.setPspCompany(po.getPspCompany());
        inst.setFee(po.getFee());
        inst.setNotificationFee(po.getNotificationFee());
        inst.setIdReceipt(po.getIdReceipt());
        inst.setIdFlowReporting(po.getIdFlowReporting());
        // substring to exclude prefix "PO_"
        inst.setStatus(InstallmentStatus.valueOf(po.getStatus().name().substring(3)));
        inst.setLastUpdatedDate(po.getLastUpdatedDate());

        List<PaymentOptionMetadata> metadata = po.getPaymentOptionMetadata();
        if (!CollectionUtils.isEmpty(metadata)) {
            for (PaymentOptionMetadata m : metadata) {
                inst.addInstallmentMetadata(InstallmentMetadataModelResponse.builder().key(m.getKey()).value(m.getValue()).build());
            }
        }

        List<Transfer> trasfers = po.getTransfer();
        if (!CollectionUtils.isEmpty(trasfers)) {
            List<TransferModelResponse> transfersModel = trasfers.stream()
                    .map(this::convertTransfer)
                    .toList();
            inst.setTransfer(transfersModel);
        }

        return inst;
    }

    private PaymentOptionModelResponseV3 convert(PaymentOption po) {
        PaymentOptionModelResponseV3 pov3 = new PaymentOptionModelResponseV3();

        pov3.setRetentionDate(po.getRetentionDate());
        pov3.setInsertedDate(po.getInsertedDate());
        // debtor
        DebtorModel debtor = new DebtorModel();
        debtor.setType(po.getType());
        debtor.setFiscalCode(po.getFiscalCode());
        debtor.setFullName(po.getFullName());
        debtor.setStreetName(po.getStreetName());
        debtor.setCivicNumber(po.getCivicNumber());
        debtor.setPostalCode(po.getPostalCode());
        debtor.setCity(po.getCity());
        debtor.setProvince(po.getProvince());
        debtor.setRegion(po.getRegion());
        debtor.setCountry(po.getCountry());
        debtor.setEmail(po.getEmail());
        debtor.setPhone(po.getPhone());

        pov3.setDebtor(debtor);

        return pov3;
    }

    public TransferModelResponse convertTransfer(Transfer t) {
        TransferModelResponse destination = new TransferModelResponse();

        destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
        destination.setCompanyName(t.getCompanyName());
        destination.setIdTransfer(t.getIdTransfer());
        destination.setAmount(t.getAmount());
        destination.setRemittanceInformation(t.getRemittanceInformation());
        destination.setCategory(t.getCategory());
        destination.setIban(t.getIban());
        destination.setPostalIban(t.getPostalIban());
        destination.setInsertedDate(t.getInsertedDate());
        destination.setStatus(t.getStatus());
        destination.setLastUpdatedDate(t.getLastUpdatedDate());

        // if one of Stamp attributes are different from null return Stamp values
        if( t.getHashDocument() != null || t.getStampType() != null || t.getProvincialResidence() != null) {
            destination.setStamp(Stamp.builder()
                    .hashDocument(t.getHashDocument())
                    .provincialResidence(t.getProvincialResidence())
                    .stampType(t.getStampType())
                    .build());
        }

        destination.setTransferMetadata(ObjectMapperUtils.mapAll(t.getTransferMetadata(), TransferMetadataModelResponse.class));

        return destination;
    }
}
