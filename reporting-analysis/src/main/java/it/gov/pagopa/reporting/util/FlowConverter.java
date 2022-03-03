package it.gov.pagopa.reporting.util;
import it.gov.pagopa.reporting.entity.FlowEntity;
import it.gov.pagopa.reporting.model.Flow;
import lombok.SneakyThrows;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.Valid;

public class FlowConverter implements Converter<FlowEntity, Flow> {

    @SneakyThrows
    @Override
    public Flow convert(MappingContext<FlowEntity, Flow> mappingContext) {
        @Valid FlowEntity source = mappingContext.getSource();

        return Flow.builder()
                .flowId(source.getRowKey())
                .flowDate(source.getFlowDate())
                .build();
    }
}
