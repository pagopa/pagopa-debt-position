package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionMetadataModelResponse;
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

        destination.setPaymentOptionMetadata(ConvertUtils.convertMetadataFromMap(source.getMetadata(), PaymentOptionMetadataModelResponse.class));

        if (source.getTransfer() != null) {
            destination.setTransfer(source.getTransfer().stream().filter(Objects::nonNull).map(ConvertUtils::convertTransfer).toList());
        } else {
            destination.setTransfer(new ArrayList<>());
        }

        return destination;
    }
}