package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConvertTransferEntityToTransferModeResponse implements Converter<Transfer, TransferModelResponse> {

    @Override
    public TransferModelResponse convert(MappingContext<Transfer, TransferModelResponse> context) {
        Transfer source = context.getSource();
        return ConvertUtils.convertTransfer(source);
    }
}