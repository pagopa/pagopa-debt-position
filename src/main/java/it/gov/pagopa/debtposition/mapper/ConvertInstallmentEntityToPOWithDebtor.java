package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import jakarta.validation.constraints.NotNull;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConvertInstallmentEntityToPOWithDebtor
        implements Converter<Installment, PaymentOptionWithDebtorInfoModelResponse> {

    @Override
    public PaymentOptionWithDebtorInfoModelResponse convert(
            MappingContext<@NotNull Installment, PaymentOptionWithDebtorInfoModelResponse> context) {
        Installment source = context.getSource();

        PaymentOptionModelResponse paymentOptionModelResponse = ObjectMapperUtils.map(source, PaymentOptionModelResponse.class);
        PaymentOptionWithDebtorInfoModelResponse destination = ObjectMapperUtils.map(paymentOptionModelResponse, PaymentOptionWithDebtorInfoModelResponse.class);

        // PaymentOption info
        destination.setServiceType(source.getPaymentPosition().getServiceType().name());
        destination.setPayStandIn(source.getPaymentPosition().getPayStandIn());

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

        destination.setDebtPositionStatus(source.getPaymentPosition().getStatus());

        return destination;
    }
}
