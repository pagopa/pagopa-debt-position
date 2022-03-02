package it.gov.pagopa.reporting.util;

import it.gov.pagopa.reporting.entity.FlowEntity;
import it.gov.pagopa.reporting.model.Flow;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
class FlowConverterTest {

    @Test
    void convert() {
        String flowId = "2022-01-12PPAYITR1XXX-S239349322";
        String flowDate = "2022-01-11T23:31:05";
        String organizationId = "idPa";

        FlowEntity flowEntity = new FlowEntity(flowId, flowDate, organizationId);

        Converter<FlowEntity, Flow> converter = new FlowConverter();
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.createTypeMap(FlowEntity.class, Flow.class).setConverter(converter);

        Flow flow = modelMapper.map(flowEntity, Flow.class);

        assertEquals(flowId, flow.getFlowId());
        assertEquals(flowDate, flow.getFlowDate());
    }
}
