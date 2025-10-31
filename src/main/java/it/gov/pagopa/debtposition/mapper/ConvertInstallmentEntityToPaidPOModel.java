package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.payments.response.PaidPaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConvertInstallmentEntityToPaidPOModel implements Converter<Installment, PaidPaymentOptionModel> {

    @Override
    public PaidPaymentOptionModel convert(MappingContext<Installment, PaidPaymentOptionModel> context) {
        Installment source = context.getSource();
        PaymentPosition paymentPosition = source.getPaymentPosition();

        PaymentOptionModelResponse paymentOptionModelResponse = ObjectMapperUtils.map(source, PaymentOptionModelResponse.class);
        PaidPaymentOptionModel destination = ObjectMapperUtils.map(paymentOptionModelResponse, PaidPaymentOptionModel.class);
        destination.setServiceType(paymentPosition.getServiceType().name());

        return destination;
    }
}