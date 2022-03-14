package it.gov.pagopa.reporting;

import it.gov.pagopa.reporting.models.FlowsMessage;
import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@ExtendWith(MockitoExtension.class)
class FlowsMessageTest {

    @Test
    void flowMessageTest() throws ParseException, DatatypeConfigurationException {

        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");

        GregorianCalendar cal2 = new GregorianCalendar();
        cal2.setTime(format.parse("2015-04-24 11:15:00"));

        TipoIdRendicontazione e1 = new TipoIdRendicontazione();
        e1.setIdentificativoFlusso(UUID.randomUUID().toString());
        GregorianCalendar cal1 = new GregorianCalendar();
        cal1.setTime(format.parse("2014-04-24 11:15:00"));
        e1.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        FlowsMessage message = new FlowsMessage();

        message.setIdPA("idPA");
        message.setFlows(new TipoIdRendicontazione[] { e1 });

        assertNotNull(message.getFlows());
        assertNotNull(message.getIdPA());
        assertEquals(1, message.getFlows().length);
    }
}
