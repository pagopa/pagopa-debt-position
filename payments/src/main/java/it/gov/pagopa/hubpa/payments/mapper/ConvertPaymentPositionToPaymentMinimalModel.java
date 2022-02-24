package it.gov.pagopa.hubpa.payments.mapper;

import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;
import it.gov.pagopa.hubpa.payments.model.PaymentMinimalModel;

public class ConvertPaymentPositionToPaymentMinimalModel implements Converter<PaymentPosition, PaymentMinimalModel> {

    @Override
    public PaymentMinimalModel convert(MappingContext<PaymentPosition, PaymentMinimalModel> context) {
	PaymentPosition source = context.getSource();
	PaymentMinimalModel destination = new PaymentMinimalModel();
	if(source.getInsertDate()!=null) {
	    destination.setDate(source.getInsertDate().toLocalDate());
	}
	destination.setFiscalCode(source.getDebitor().getFiscalCode());
	destination.setId(source.getId());
	destination.setName(source.getDebitor().getName());
	destination.setStatus(source.getStatus());
	destination.setSurname(source.getDebitor().getSurname());
	destination.setIsDuplicated("POSSIBLE_DUPLICATE".equals(source.getInformation())?Boolean.TRUE:Boolean.FALSE);

	return destination;
    }

    

}
