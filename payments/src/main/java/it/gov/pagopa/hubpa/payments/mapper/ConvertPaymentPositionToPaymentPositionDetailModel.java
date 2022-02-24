package it.gov.pagopa.hubpa.payments.mapper;

import java.util.Comparator;
import java.util.List;

import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import it.gov.pagopa.hubpa.payments.entity.Debitor;
import it.gov.pagopa.hubpa.payments.entity.PaymentOptions;
import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;
import it.gov.pagopa.hubpa.payments.model.InstallmentDetailModel;
import it.gov.pagopa.hubpa.payments.model.PaymentPositionDetailModel;

public class ConvertPaymentPositionToPaymentPositionDetailModel implements Converter<PaymentPosition, PaymentPositionDetailModel> {

    @Override
    public PaymentPositionDetailModel convert(MappingContext<PaymentPosition, PaymentPositionDetailModel> context) {
	PaymentPosition source = context.getSource();
	PaymentPositionDetailModel destination = new PaymentPositionDetailModel();
	Debitor debitor=source.getDebitor();
	if(debitor!=null) {
	    destination.setNominative(debitor.getName()+" "+debitor.getSurname());
	    destination.setFiscalCode(debitor.getFiscalCode());
	    destination.setAddressLine1(debitor.getAddress()+" "+debitor.getNumber());
	    destination.setAddressLine2(debitor.getCap()+" "+debitor.getArea());
	    destination.setStatus(source.getStatus());
	    destination.setPublishDate(source.getPublishDate());
	    this.addInstallments(source.getPaymentOptions(),destination);
	}
	return destination;
    }

    private void addInstallments(List<PaymentOptions> paymentOptionsList, PaymentPositionDetailModel paymentPositions) {
	if(paymentOptionsList!=null) {
	    
	    paymentOptionsList.sort(Comparator.comparing(PaymentOptions::getIsConclusive)
		    .thenComparing(PaymentOptions::getDuoDate));
	    
	    for(PaymentOptions paymentOptions:paymentOptionsList) {
		InstallmentDetailModel inst=new InstallmentDetailModel();
		inst.setAmount(paymentOptions.getAmount());
		inst.setDueDate(paymentOptions.getDuoDate());
		inst.setNotificationCode(paymentOptions.getNotificationCode());
		inst.setStatus(paymentOptions.getStatus());
		inst.setIsConclusive(paymentOptions.getIsConclusive());
		paymentPositions.getInstallments().add(inst);
		
		if(paymentOptions.getTransfers()!=null && !paymentOptions.getTransfers().isEmpty()) {
		    paymentPositions.setDescription(paymentOptions.getTransfers().get(0).getReason());
		}
		
	    }
	}
	
    }

}
