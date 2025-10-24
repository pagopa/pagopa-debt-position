package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;
import java.util.Objects;

public class ConvertInstallmentEntityToPOModelResponse implements Converter<Installment, PaymentOptionModelResponse> {

    @Override
    public PaymentOptionModelResponse convert(MappingContext<Installment, PaymentOptionModelResponse> context) {
        Installment source = context.getSource();

        PaymentOptionModelResponse destination = new PaymentOptionModelResponse();
        destination.setNav(source.getNav());
        destination.setIuv(source.getIuv());
        destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destination.setAmount(source.getAmount());
        destination.setDescription(source.getDescription());
        destination.setIsPartialPayment(OptionType.OPZIONE_RATEALE.equals(source.getPaymentOption().getOptionType()));
        destination.setDueDate(source.getDueDate());
        destination.setRetentionDate(source.getPaymentOption().getRetentionDate());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setReportingDate(source.getReportingDate());
        destination.setInsertedDate(source.getInsertedDate());
        destination.setPaymentMethod(source.getPaymentMethod());
        destination.setFee(source.getFee());
        destination.setNotificationFee(source.getNotificationFee());
        destination.setPspCompany(source.getPspCompany());
        destination.setIdReceipt(source.getReceiptId());
        destination.setIdFlowReporting(source.getFlowReportingId());
        destination.setStatus(ObjectMapperUtils.mapInstallmentStatusToPoStatus(source.getStatus()));
        destination.setLastUpdatedDate(source.getLastUpdatedDate());
        destination.setLastUpdatedDateNotificationFee(source.getLastUpdatedDateNotificationFee());

        destination.setPaymentOptionMetadata(ObjectMapperUtils.mapAll(source.getMetadata(), PaymentOptionMetadataModelResponse.class));

        if (source.getTransfer() != null) {
            destination.setTransfer(source.getTransfer().stream().filter(Objects::nonNull).map(this::convertTransfer).toList());
        } else {
            destination.setTransfer(new ArrayList<>());
        }

        return destination;
    }

    private TransferModelResponse convertTransfer(Transfer t) {
        TransferModelResponse destination = new TransferModelResponse();

        destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
        destination.setCompanyName(t.getInstallment().getPaymentPosition().getCompanyName());
        destination.setIdTransfer(t.getTransferId());
        destination.setAmount(t.getAmount());
        destination.setRemittanceInformation(t.getRemittanceInformation());
        destination.setCategory(t.getCategory());
        destination.setIban(t.getIban());
        destination.setPostalIban(t.getPostalIban());

        // if one of Stamp attributes are different from null return Stamp values
        if (t.getHashDocument() != null
                || t.getStampType() != null
                || t.getProvincialResidence() != null) {
            destination.setStamp(
                    Stamp.builder()
                            .hashDocument(t.getHashDocument())
                            .provincialResidence(t.getProvincialResidence())
                            .stampType(t.getStampType())
                            .build());
        }
        destination.setInsertedDate(t.getInsertedDate());
        destination.setStatus(t.getStatus());
        destination.setLastUpdatedDate(t.getLastUpdatedDate());

        destination.setTransferMetadata(ObjectMapperUtils.mapAll(t.getMetadata(), TransferMetadataModelResponse.class));

        return destination;
    }
}