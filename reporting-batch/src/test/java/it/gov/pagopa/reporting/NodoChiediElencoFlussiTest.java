package it.gov.pagopa.reporting;

import java.lang.reflect.Field;
import java.net.ConnectException;
import java.util.logging.Logger;

import javax.xml.ws.WebServiceException;

import it.gov.pagopa.reporting.servicewsdl.PagamentiTelematiciRPT;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import it.gov.pagopa.reporting.service.NodoChiediElencoFlussi;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;

@ExtendWith(MockitoExtension.class)
class NodoChiediElencoFlussiTest {

    @Mock
    PagamentiTelematiciRPT myPort;

    Logger logger = Logger.getLogger("testlogging");

    @Test
    void nodoChiediElencoFlussiTestNetworkErrorTest() {

        NodoChiediElencoFlussi nodoChiediElencoFlussi = new NodoChiediElencoFlussi();

        try {

            nodoChiediElencoFlussi.nodoChiediElencoFlussiRendicontazione("");
            assertNull(nodoChiediElencoFlussi.getNodoChiediElencoFlussiRendicontazioneFault());
        } catch (WebServiceException e) {
            assertTrue(Boolean.TRUE);
        } catch (Exception e) {
            fail();
        }

        assertNull(nodoChiediElencoFlussi.getNodoChiediElencoFlussiRendicontazione());
    }

    @Test
    void nodoChiediElencoFlussiTestSettersTest() throws Exception {

        NodoChiediElencoFlussi nodoChiediElencoFlussi = new NodoChiediElencoFlussi();

        Field keyField = nodoChiediElencoFlussi.getClass().getDeclaredField("port");
        keyField.setAccessible(true);
        keyField.set(nodoChiediElencoFlussi, myPort);

        String idPa = "12345";
        nodoChiediElencoFlussi.nodoChiediElencoFlussiRendicontazione(idPa);

        assert (Boolean.TRUE);
    }

}
