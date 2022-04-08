package it.gov.pagopa.payments.mapper;

import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.model.ReceiptModelResponse;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.Valid;

public class ConvertReceiptEntityToReceiptModelResponse implements Converter<ReceiptEntity, ReceiptModelResponse> {

    @Override
    public ReceiptModelResponse convert(MappingContext<ReceiptEntity, ReceiptModelResponse> mappingContext) {
        @Valid ReceiptEntity re = mappingContext.getSource();
        return ReceiptModelResponse.builder()
                .organizationFiscalCode(re.getPartitionKey())
                .iuv(re.getRowKey())
                .debtorFiscalCode(re.getDebtor())
                .build();
    }
}
