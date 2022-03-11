package it.gov.pagopa.reporting;

import com.microsoft.azure.storage.StorageExtendedErrorInformation;
import com.microsoft.azure.storage.table.TableServiceException;
import it.gov.pagopa.reporting.service.FlowsService;
import it.gov.pagopa.reporting.servicewsdl.TipoElencoFlussiRendicontazione;
import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.xml.datatype.DatatypeFactory;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;
import java.util.UUID;
import java.util.logging.Logger;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class FlowServiceTest {

    @Test
    void flowsProcessingTest() throws Exception {

        /**
         * Mock input - identical flows
         */
        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");

        GregorianCalendar cal2 = new GregorianCalendar();
        cal2.setTime(format.parse("2015-04-24 11:15:00"));

        TipoIdRendicontazione e1 = new TipoIdRendicontazione();
        e1.setIdentificativoFlusso(UUID.randomUUID().toString());
        GregorianCalendar cal1 = new GregorianCalendar();
        cal1.setTime(format.parse("2014-04-24 11:15:00"));
        e1.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        TipoElencoFlussiRendicontazione elencoFlussi = new TipoElencoFlussiRendicontazione();
        elencoFlussi.setTotRestituiti(2);
        elencoFlussi.getIdRendicontazione().add(e1);
        elencoFlussi.getIdRendicontazione().add(e1);

        FlowsService flowsService = Mockito.spy(
                new FlowsService("connectionStringMock", "tableMock", "queueMock", Logger.getLogger("testlogging")));

        /**
         * Precondition
         */
        doThrow(new TableServiceException("InvalidDuplicateRow", "message InvalidDuplicateRow", 400,
                new StorageExtendedErrorInformation(), null)).when(flowsService).flowsBatchProcessing(any(),
                        anyString(), anyInt());
        doNothing().when(flowsService).flowProcessing(any(), anyString());

        /**
         * Test
         */
        flowsService.flowsProcessing(elencoFlussi.getIdRendicontazione(), "idPaMock");

        /**
         * Asserts
         */
        verify(flowsService, times(1)).flowsBatchProcessing(any(), anyString(), anyInt());
        verify(flowsService, times(2)).flowProcessing(any(), anyString());
    }

}
