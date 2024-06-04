package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.PaymentPositionModelBaseResponse;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;
import java.util.List;

public class ConvertPPToPPResponse implements Converter<PaymentPosition, PaymentPositionModelBaseResponse> {
    @Override
    public PaymentPositionModelBaseResponse convert(MappingContext<PaymentPosition, PaymentPositionModelBaseResponse> context) {
        PaymentPosition source = context.getSource();
        PaymentPositionModelBaseResponse destination = new PaymentPositionModelBaseResponse();

        destination.setIupd(source.getIupd());
        destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destination.setType(source.getType());
        destination.setPull(source.getPull());
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setInsertedDate(source.getInsertedDate());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setPublishDate(source.getPublishDate());
        destination.setValidityDate(source.getValidityDate());
        destination.setLastUpdatedDate(source.getLastUpdatedDate());
        destination.setStatus(source.getStatus());

        List<PaymentOptionModelResponse> list = new ArrayList<>();
        source.getPaymentOption().forEach(
                p -> list.add(ConvertPOEntityToPOResponse.convert(p)));
        destination.setPaymentOption(list);

        return destination;
    }
}
