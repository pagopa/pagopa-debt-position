package it.gov.pagopa.util;
import it.gov.pagopa.reporting.entity.FlowEntity;
import it.gov.pagopa.reporting.model.Flow;
import lombok.SneakyThrows;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.Valid;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

public class FlowConverter implements Converter<FlowEntity, Flow> {

    @SneakyThrows
    @Override
    public Flow convert(MappingContext<FlowEntity, Flow> mappingContext) {
        @Valid FlowEntity source = mappingContext.getSource();

//        DateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
//        Date date = format.parse(source.getFlowDate());
//        GregorianCalendar cal = new GregorianCalendar();
//        cal.setTimeZone(TimeZone.getTimeZone("CEST"));
//        cal.setTime(date);
//        XMLGregorianCalendar xmlGregCalendar = DatatypeFactory.newInstance().newXMLGregorianCalendar(cal);
        return Flow.builder()
                .flowId(source.getRowKey())
                .flowDate(source.getFlowDate())
                .build();
    }
}
