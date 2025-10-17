package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.payments.response.PaidPaymentOptionModel;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConvertPOEntityToPaidPOModel implements Converter<Installment, PaidPaymentOptionModel> {

    @Override
    public PaidPaymentOptionModel convert(MappingContext<Installment, PaidPaymentOptionModel> context) {
        Installment source = context.getSource();
        PaymentPosition paymentPosition = source.getPaymentPosition();

        PaidPaymentOptionModel destination = ObjectMapperUtils.map(source, PaidPaymentOptionModel.class);
        destination.setServiceType(paymentPosition.getServiceType().name());

        return destination;
    }
}