package it.gov.pagopa.debtposition.controller.pd.mapper;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.TransferModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.constraints.NotNull;

public class ConvertPOEntityToPOWithDebtor implements Converter<PaymentOption, PaymentOptionWithDebtorInfoModelResponse> {

    @Override
    public PaymentOptionWithDebtorInfoModelResponse convert(MappingContext<@NotNull PaymentOption, PaymentOptionWithDebtorInfoModelResponse> context) {
        PaymentOption source = context.getSource();
        PaymentOptionWithDebtorInfoModelResponse destination = new PaymentOptionWithDebtorInfoModelResponse();


        destination.setIuv(source.getIuv());
        destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destination.setAmount(source.getAmount());
        destination.setDescription(source.getDescription());
        destination.setIsPartialPayment(source.getIsPartialPayment());
        destination.setDueDate(source.getDueDate());
        destination.setRetentionDate(source.getRetentionDate());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setReportingDate(source.getReportingDate());
        destination.setPaymentMethod(source.getPaymentMethod());
        destination.setFee(source.getFee());
        destination.setPspCompany(source.getPspCompany());
        destination.setIdReceipt(source.getIdReceipt());
        destination.setIdFlowReporting(source.getIdFlowReporting());
        destination.setStatus(source.getStatus());


        destination.setCity(source.getPaymentPosition().getCity());
        destination.setCivicNumber(source.getPaymentPosition().getCivicNumber());
        destination.setCompanyName(source.getPaymentPosition().getCompanyName());
        destination.setCountry(source.getPaymentPosition().getCountry());
        destination.setEmail(source.getPaymentPosition().getEmail());
        destination.setFiscalCode(source.getPaymentPosition().getFiscalCode());
        destination.setFullName(source.getPaymentPosition().getFullName());
        destination.setOfficeName(source.getPaymentPosition().getOfficeName());
        destination.setPhone(source.getPaymentPosition().getPhone());
        destination.setPostalCode(source.getPaymentPosition().getPostalCode());
        destination.setProvince(source.getPaymentPosition().getProvince());
        destination.setRegion(source.getPaymentPosition().getRegion());
        destination.setStreetName(source.getPaymentPosition().getStreetName());
        destination.setType(source.getPaymentPosition().getType());
        destination.setDebtPositionStatus(source.getPaymentPosition().getStatus());

        destination.setTransfer(ObjectMapperUtils.mapAll(source.getTransfer(), TransferModelResponse.class));


        return destination;
    }

}
