package it.gov.pagopa.hubpa.payments.mapper;

import java.util.List;

import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import it.gov.pagopa.hubpa.payments.entity.Debitor;
import it.gov.pagopa.hubpa.payments.entity.PaymentOptions;
import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;
import it.gov.pagopa.hubpa.payments.entity.Transfers;
import it.gov.pagopa.hubpa.payments.model.CsvPositionModel;

public class ConvertPaymentPositionDebitorToCsvPositionModel implements Converter<PaymentPosition, CsvPositionModel> {

    @Override
    public CsvPositionModel convert(MappingContext<PaymentPosition, CsvPositionModel> context) {
	PaymentPosition paymentPosition = context.getSource();
	CsvPositionModel destination = new CsvPositionModel();

	Debitor debitor = paymentPosition.getDebitor();

	destination.setAddress(debitor.getAddress());
	destination.setArea(debitor.getArea());
	destination.setCap(debitor.getCap());
	destination.setCountry(debitor.getCountry());
	destination.setEmail(debitor.getEmail());
	destination.setFiscalCode(debitor.getFiscalCode());
	destination.setIdTenant(debitor.getIdTenant());
	destination.setName(debitor.getName());
	destination.setNumber(debitor.getNumber());
	destination.setPhone(debitor.getPhone());
	destination.setProvince(debitor.getProvince());
	destination.setSurname(debitor.getSurname());
	destination.setType(debitor.getType());
	destination.setInformation(paymentPosition.getInformation());
	destination.setAmount(paymentPosition.getAmount());
	List<PaymentOptions> paymentOptions = paymentPosition.getPaymentOptions();
	if (paymentOptions != null && !paymentOptions.isEmpty()) {
	    List<Transfers> transfers = paymentOptions.get(0).getTransfers();
	    if (transfers != null && !transfers.isEmpty()) {
		destination.setReason(transfers.get(0).getReason());
	    }
	}
	
	return destination;
    }

}
