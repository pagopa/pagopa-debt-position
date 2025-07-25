package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.payments.response.PaidPaymentOptionModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConvertPOEntityToPaidPOModel implements Converter<PaymentOption, PaidPaymentOptionModelResponse> {

    @Override
    public PaidPaymentOptionModelResponse convert(MappingContext<PaymentOption, PaidPaymentOptionModelResponse> context) {
        PaymentOption source = context.getSource();
        PaymentPosition paymentPosition = source.getPaymentPosition();

        PaidPaymentOptionModelResponse destination = ObjectMapperUtils.map(source, PaidPaymentOptionModelResponse.class);
        destination.setServiceType(paymentPosition.getServiceType().name());

        return destination;
    }
}
