package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;
import java.util.List;

public class ConvertPPToPPModel implements Converter<PaymentPosition, PaymentPositionModel> {

    @Override
    public PaymentPositionModel convert(MappingContext<PaymentPosition, PaymentPositionModel> context) {
        PaymentPosition source = context.getSource();
        PaymentPositionModel destination = new PaymentPositionModel();

        destination.setIupd(source.getIupd());
        destination.setType(source.getType());
        destination.setPull(source.getPull());
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setValidityDate(source.getValidityDate());
        destination.setStatus(source.getStatus());
        destination.setSwitchToExpired(source.getSwitchToExpired());

        destination.setCity(source.getCity());
        destination.setCivicNumber(source.getCivicNumber());
        destination.setCountry(source.getCountry());
        destination.setEmail(source.getEmail());
        destination.setFiscalCode(source.getFiscalCode());
        destination.setFullName(source.getFullName());
        destination.setPhone(source.getPhone());
        destination.setPostalCode(source.getPostalCode());
        destination.setProvince(source.getProvince());
        destination.setRegion(source.getRegion());
        destination.setStreetName(source.getStreetName());

        List<PaymentOptionModel> list = new ArrayList<>();
        source.getPaymentOption().forEach(
                p -> list.add(ConvertPOEntityToPOModel.convert(p)));
        destination.setPaymentOption(list);

        return destination;
    }
}
