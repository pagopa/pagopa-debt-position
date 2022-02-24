package it.gov.pagopa.hubpa.payments.mapper;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;

import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import it.gov.pagopa.hubpa.payments.entity.Debitor;
import it.gov.pagopa.hubpa.payments.entity.PaymentOptions;
import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;
import it.gov.pagopa.hubpa.payments.entity.Transfers;
import it.gov.pagopa.hubpa.payments.model.DebitorModel;
import it.gov.pagopa.hubpa.payments.model.PaymentOptionsModel;
import it.gov.pagopa.hubpa.payments.model.PaymentPositionModel;
import it.gov.pagopa.hubpa.payments.model.TransfersModel;

public class ConvertDebitorModelToDebitor implements Converter<DebitorModel, Debitor> {

    @Override
    public Debitor convert(MappingContext<DebitorModel, Debitor> context) {
	DebitorModel source = context.getSource();
	Debitor destination = new Debitor();

	destination.setAddress(source.getAddress());
	destination.setArea(source.getArea());
	destination.setCap(source.getCap());
	destination.setCountry(source.getCountry());
	destination.setEmail(source.getEmail());
	destination.setFiscalCode(source.getFiscalCode());
	destination.setIdTenant(source.getIdTenant());
	destination.setName(source.getName());
	destination.setNumber(source.getNumber());
	destination.setPhone(source.getPhone());
	destination.setProvince(source.getProvince());
	destination.setSurname(source.getSurname());
	destination.setType(source.getType());

	List<PaymentPositionModel> paymentPositions = source.getPaymentPosition();
	if (paymentPositions != null) {
	    for (PaymentPositionModel paymentPositionModel : paymentPositions) {
		destination.addPaymentPosition(this.convertPaymentPosition(paymentPositionModel));
	    }
	}

	return destination;
    }

    private PaymentPosition convertPaymentPosition(PaymentPositionModel paymentPositionModel) {
	PaymentPosition paymentPosition = new PaymentPosition();
	paymentPosition.setCompanyName(paymentPositionModel.getCompanyName());
	paymentPosition.setDescription(paymentPositionModel.getDescription());
	paymentPosition.setJobId(paymentPositionModel.getJobId());
	paymentPosition.setOfficeName(paymentPositionModel.getOfficeName());
	paymentPosition.setOrganizationFiscalCode(paymentPositionModel.getOrganizationFiscalCode());
	paymentPosition.setStatus(paymentPositionModel.getStatus());
	paymentPosition.setInsertDate(LocalDateTime.now(ZoneId.of("Europe/Paris")));
	paymentPosition.setAmount(paymentPositionModel.getAmount());
	paymentPosition.setTotalOptions(paymentPositionModel.getTotalOptions());
	paymentPosition.setPaidOptions(paymentPositionModel.getPaidOptions());
	paymentPosition.setReportedOptions(paymentPositionModel.getReportedOptions());

	List<PaymentOptionsModel> paymentOptions = paymentPositionModel.getPaymentOptions();
	if (paymentOptions != null) {
	    for (PaymentOptionsModel paymentOptionsModel : paymentOptions) {
		paymentPosition.addPaymentOptions(this.convertPaymentOptions(paymentOptionsModel));
	    }
	}

	return paymentPosition;
    }

    private PaymentOptions convertPaymentOptions(PaymentOptionsModel paymentOptionsModel) {
	PaymentOptions paymentOptions = new PaymentOptions();
	paymentOptions.setAmount(paymentOptionsModel.getAmount());
	paymentOptions.setFiscalCode(paymentOptionsModel.getFiscalCode());
	paymentOptions.setDuoDate(paymentOptionsModel.getDuoDate());
	paymentOptions.setIsConclusive(paymentOptionsModel.getIsConclusive());
	paymentOptions.setMetadata(paymentOptionsModel.getMetadata());
	paymentOptions.setRetentionDate(paymentOptionsModel.getRetentionDate());
	paymentOptions.setStatus(paymentOptionsModel.getStatus());
	paymentOptions.setAllCpp(paymentOptionsModel.getAllCpp());
	paymentOptions.setIdFlowReporting(paymentOptionsModel.getIdFlowReporting());
	paymentOptions.setDateReporting(paymentOptionsModel.getDateReporting());

	List<TransfersModel> transfers = paymentOptionsModel.getTransfers();
	if (transfers != null) {
	    for (TransfersModel transfersModel : transfers) {
		paymentOptions.addTransfers(this.convertTransfers(transfersModel));
	    }
	}

	return paymentOptions;
    }

    private Transfers convertTransfers(TransfersModel transfersModel) {
	Transfers transfers = new Transfers();
	transfers.setIban(transfersModel.getIban());
	transfers.setOrganizationFiscalCode(transfersModel.getOrganizationFiscalCode());
	transfers.setPartialAmount(transfersModel.getPartialAmount());
	transfers.setReason(transfersModel.getReason());
	transfers.setTaxonomy(transfersModel.getTaxonomy());
	transfers.setPostalIban(transfersModel.getPostalIban());
	transfers.setPostalIbanHolder(transfersModel.getPostalIbanHolder());
	transfers.setPostalAuthCode(transfersModel.getPostalAuthCode());
	return transfers;
    }

}
