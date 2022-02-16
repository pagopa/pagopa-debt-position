package it.gov.pagopa.reporting;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.security.cert.CertificateException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.datatype.DatatypeFactory;

import com.microsoft.azure.functions.ExecutionContext;

import it.gov.pagopa.reporting.service.FlowsService;
import it.gov.pagopa.reporting.service.NodoChiediElencoFlussi;
import it.gov.pagopa.reporting.servicewsdl.ObjectFactory;
import it.gov.pagopa.reporting.servicewsdl.TipoElencoFlussiRendicontazione;
import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class FunctionTest {

    @Spy
    Function function;

    @Mock
    ExecutionContext context;

    @Mock
    NodoChiediElencoFlussi nodeClient;

    @Mock
    FlowsService flowsService;

    ObjectFactory objectFactory = new ObjectFactory();

//    @Test
//    void runOkTest() throws Exception {
//
//        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
//        Date date1 = format.parse("2014-04-24 11:15:00");
//        GregorianCalendar cal1 = new GregorianCalendar();
//        cal1.setTime(date1);
//
//        TipoElencoFlussiRendicontazione elencoFlussi = new TipoElencoFlussiRendicontazione();
//        elencoFlussi.setTotRestituiti(1);
//
//        TipoIdRendicontazione e1 = new TipoIdRendicontazione();
//        String id1 = UUID.randomUUID().toString();
//        e1.setIdentificativoFlusso(id1);
//        e1.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
//                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));
//
//        elencoFlussi.getIdRendicontazione().add(e1);
//
//        // general var
//        Logger logger = Logger.getLogger("testlogging");
//
//        // precondition
//        when(context.getLogger()).thenReturn(logger);
//
//        doReturn(nodeClient).when(function).getNodeClientInstance();
//        doReturn(flowsService).when(function).getFlowsServiceInstance(logger);
//
//        doNothing().when(nodeClient).setSslContext();
//
//        doNothing().when(nodeClient).nodoChiediElencoFlussiRendicontazione(anyString());
//        when(nodeClient.getNodoChiediElencoFlussiRendicontazioneFault()).thenReturn(null);
//        when(nodeClient.getNodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione()).thenReturn(elencoFlussi);
//        // test
//        function.run("timerInfo", context);
//
//        // Asserts
//        verify(context, times(1)).getLogger();
//        verify(nodeClient, times(1)).nodoChiediElencoFlussiRendicontazione(anyString());
//    }
//
//    @Test
//    void runKoTest() throws Exception {
//
//        // general var
//        Logger logger = Logger.getLogger("testlogging");
//
//        // precondition
//        when(context.getLogger()).thenReturn(logger);
//
//        doReturn(nodeClient).when(function).getNodeClientInstance();
//        doReturn(flowsService).when(function).getFlowsServiceInstance(logger);
//
//        doNothing().when(nodeClient).setSslContext();
//
//        doNothing().when(nodeClient).nodoChiediElencoFlussiRendicontazione(anyString());
//        when(nodeClient.getNodoChiediElencoFlussiRendicontazioneFault()).thenReturn(objectFactory.createFaultBean());
//        when(nodeClient.getNodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione()).thenReturn(null);
//        // test
//        function.run("timerInfo", context);
//
//        // Asserts
//        verify(context, times(1)).getLogger();
//        verify(nodeClient, times(1)).nodoChiediElencoFlussiRendicontazione(anyString());
//    }
//
//    @Test
//    void runInvalidSslContextTest() throws Exception {
//
//        // general var
//        Logger logger = Logger.getLogger("testlogging");
//
//        // precondition
//        when(context.getLogger()).thenReturn(logger);
//
//        doReturn(nodeClient).when(function).getNodeClientInstance();
//        doReturn(flowsService).when(function).getFlowsServiceInstance(logger);
//
//        doThrow(CertificateException.class).when(nodeClient).setSslContext();
//
//        // test
//        function.run("timerInfo", context);
//
//        verify(context, times(1)).getLogger();
//        verify(nodeClient, times(0)).nodoChiediElencoFlussiRendicontazione(anyString());
//    }
//
//    @Test
//    void getFlowServiceIstanceTest() throws Exception {
//
//        Logger logger = Logger.getLogger("testlogging");
//
//        // test
//        FlowsService istance = function.getFlowsServiceInstance(logger);
//
//        assertNotNull(istance);
//    }

}
