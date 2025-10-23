package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.response.*;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class ConvertPPEntityToModelResponse
        implements Converter<PaymentPosition, PaymentPositionModelBaseResponse> {

    @Override
    public PaymentPositionModelBaseResponse convert(
            MappingContext<PaymentPosition, PaymentPositionModelBaseResponse> context) {
        PaymentPosition source = context.getSource();
        PaymentPositionModelBaseResponse destination = new PaymentPositionModelBaseResponse();

        destination.setIupd(source.getIupd());
        destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setInsertedDate(source.getInsertedDate());
        destination.setPublishDate(source.getPublishDate());
        destination.setValidityDate(source.getValidityDate());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setStatus(source.getStatus());
        destination.setLastUpdatedDate(source.getLastUpdatedDate());

        if (source.getPaymentOption() != null) {
            PaymentOption sourceFirstPO = source.getPaymentOption().get(0);
            destination.setType(sourceFirstPO.getDebtorType());

            destination.setPaymentOption(convertPaymentOptions(source.getPaymentOption()));
        } else {
            destination.setPaymentOption(new ArrayList<>());
        }

        return destination;
    }

    private List<PaymentOptionModelResponse> convertPaymentOptions(List<PaymentOption> sourcePoList) {
        List<PaymentOptionModelResponse> destinationPoList = new ArrayList<>();

        for (PaymentOption sourcePO : sourcePoList) {
            for (Installment sourceInst : sourcePO.getInstallment()) {
                PaymentOptionModelResponse destinationPO = new PaymentOptionModelResponse();

                destinationPO.setNav(sourceInst.getNav());
                destinationPO.setIuv(sourceInst.getIuv());
                destinationPO.setOrganizationFiscalCode(sourceInst.getOrganizationFiscalCode());
                destinationPO.setAmount(sourceInst.getAmount());
                destinationPO.setDescription(sourcePO.getDescription());
                destinationPO.setIsPartialPayment(OptionType.OPZIONE_RATEALE.equals(sourcePO.getOptionType()));
                destinationPO.setDueDate(sourceInst.getDueDate());
                destinationPO.setRetentionDate(sourcePO.getRetentionDate());
                destinationPO.setPaymentDate(sourceInst.getPaymentDate());
                destinationPO.setReportingDate(sourceInst.getReportingDate());
                destinationPO.setInsertedDate(sourceInst.getInsertedDate());
                destinationPO.setPaymentMethod(sourceInst.getPaymentMethod());
                destinationPO.setFee(sourceInst.getFee());
                destinationPO.setNotificationFee(sourceInst.getNotificationFee());
                destinationPO.setPspCompany(sourceInst.getPspCompany());
                destinationPO.setIdReceipt(sourceInst.getReceiptId());
                destinationPO.setIdFlowReporting(sourceInst.getFlowReportingId());
                destinationPO.setStatus(ObjectMapperUtils.mapInstallmentStatusToPoStatus(sourceInst.getStatus()));
                destinationPO.setLastUpdatedDate(sourceInst.getLastUpdatedDate());

                destinationPO.setPaymentOptionMetadata(ObjectMapperUtils.mapAll(sourceInst.getMetadata(), PaymentOptionMetadataModelResponse.class));


                destinationPO.setTransfer(sourceInst.getTransfer().stream().filter(Objects::nonNull).map(this::convertTransfer).toList());

                destinationPoList.add(destinationPO);
            }
        }
        return destinationPoList;
    }

    private TransferModelResponse convertTransfer(Transfer sourceTransfer) {
        TransferModelResponse destination = new TransferModelResponse();

        destination.setOrganizationFiscalCode(sourceTransfer.getOrganizationFiscalCode());
        destination.setCompanyName(sourceTransfer.getInstallment().getPaymentPosition().getCompanyName());
        destination.setIdTransfer(sourceTransfer.getTransferId());
        destination.setAmount(sourceTransfer.getAmount());
        destination.setRemittanceInformation(sourceTransfer.getRemittanceInformation());
        destination.setCategory(sourceTransfer.getCategory());
        destination.setIban(sourceTransfer.getIban());
        destination.setPostalIban(sourceTransfer.getPostalIban());
        // if one of Stamp attributes are different from null return Stamp values
        if (sourceTransfer.getHashDocument() != null
                || sourceTransfer.getStampType() != null
                || sourceTransfer.getProvincialResidence() != null) {
            destination.setStamp(
                    Stamp.builder()
                            .hashDocument(sourceTransfer.getHashDocument())
                            .provincialResidence(sourceTransfer.getProvincialResidence())
                            .stampType(sourceTransfer.getStampType())
                            .build());
        }

        destination.setInsertedDate(sourceTransfer.getInsertedDate());
        destination.setStatus(sourceTransfer.getStatus());
        destination.setLastUpdatedDate(sourceTransfer.getLastUpdatedDate());

        destination.setTransferMetadata(ObjectMapperUtils.mapAll(sourceTransfer.getMetadata(), TransferMetadataModelResponse.class));

        return destination;
    }
}
