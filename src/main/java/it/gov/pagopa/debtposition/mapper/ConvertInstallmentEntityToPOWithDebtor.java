package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import jakarta.validation.constraints.NotNull;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;

public class ConvertInstallmentEntityToPOWithDebtor
        implements Converter<Installment, PaymentOptionWithDebtorInfoModelResponse> {

    @Override
    public PaymentOptionWithDebtorInfoModelResponse convert(
            MappingContext<@NotNull Installment, PaymentOptionWithDebtorInfoModelResponse> context) {
        Installment source = context.getSource();

        PaymentOptionWithDebtorInfoModelResponse destination =
                new PaymentOptionWithDebtorInfoModelResponse();

        // PaymentOption info
        destination.setNav(source.getNav());
        destination.setIuv(source.getIuv());
        destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destination.setAmount(source.getAmount());
        destination.setDescription(source.getDescription());
        destination.setIsPartialPayment(OptionType.OPZIONE_RATEALE.equals(source.getPaymentOption().getOptionType()));
        destination.setPayStandIn(source.getPaymentPosition().getPayStandIn());
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
        destination.setServiceType(source.getPaymentPosition().getServiceType().name());
        destination.setStatus(ObjectMapperUtils.mapInstallmentStatusToPoStatus(source.getStatus()));

        // TODO metadata
//    destination.setPaymentOptionMetadata(
//        ObjectMapperUtils.mapAll(
//            source.getPaymentOptionMetadata(), PaymentOptionMetadataModelResponse.class));

        // PaymentPosition creditor info
        destination.setIupd(source.getPaymentPosition().getIupd());
        destination.setType(source.getPaymentOption().getDebtorType());
        destination.setFiscalCode(source.getPaymentOption().getDebtorFiscalCode());
        destination.setFullName(source.getPaymentOption().getDebtorFullName());
        destination.setStreetName(source.getPaymentOption().getDebtorStreetName());
        destination.setCivicNumber(source.getPaymentOption().getDebtorCivicNumber());
        destination.setPostalCode(source.getPaymentOption().getDebtorFiscalCode());
        destination.setCity(source.getPaymentOption().getDebtorCity());
        destination.setProvince(source.getPaymentOption().getDebtorProvince());
        destination.setRegion(source.getPaymentOption().getDebtorRegion());
        destination.setCountry(source.getPaymentOption().getDebtorCountry());
        destination.setEmail(source.getPaymentOption().getDebtorEmail());
        destination.setPhone(source.getPaymentOption().getDebtorPhone());
        destination.setCompanyName(source.getPaymentPosition().getCompanyName());
        destination.setOfficeName(source.getPaymentPosition().getOfficeName());

        destination.setDebtPositionStatus(ObjectMapperUtils.mapPpStatusV3ToPpStatus(source.getPaymentPosition().getStatus()));

        if (source.getTransfer() != null) {
            destination.setTransfer(source.getTransfer().stream().map(this::convertTransfer).toList());
        } else {
            destination.setTransfer(new ArrayList<>());
        }

        return destination;
    }

    private TransferModelResponse convertTransfer(Transfer t) {
        TransferModelResponse destination = new TransferModelResponse();

        if (null == t) return destination;

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

        // TODO metadata transferModel
//    destination.setTransferMetadata(
//        ObjectMapperUtils.mapAll(t.getTransferMetadata(), TransferMetadataModel.class));
//
//    List<TransferMetadataModel> transferMetadataModelResponses =
//        convertTransferMetadata(t.getTransferMetadata());
//    destination.setTransferMetadata(transferMetadataModelResponses);

        return destination;
    }
}
