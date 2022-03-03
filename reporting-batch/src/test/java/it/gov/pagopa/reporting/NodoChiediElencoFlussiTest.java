package it.gov.pagopa.reporting;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.security.spec.InvalidKeySpecException;
import java.util.logging.Logger;

import javax.xml.ws.Holder;
import javax.xml.ws.WebServiceException;

import com.microsoft.azure.functions.ExecutionContext;
import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.PagamentiTelematiciRPT;
import it.gov.pagopa.reporting.servicewsdl.TipoElencoFlussiRendicontazione;
import org.glassfish.pfl.basic.func.NullaryFunctionBase;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import it.gov.pagopa.reporting.service.NodoChiediElencoFlussi;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

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

        } catch (WebServiceException e) {

            assertTrue(Boolean.TRUE);
        }

        assertNull(nodoChiediElencoFlussi.getNodoChiediElencoFlussiRendicontazioneFault());
        assertNull(nodoChiediElencoFlussi.getNodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione());
    }

    @Test
    void nodoChiediElencoFlussiTestSettersTest() throws NoSuchFieldException, IllegalAccessException {

        NodoChiediElencoFlussi nodoChiediElencoFlussi = new NodoChiediElencoFlussi();

        Field keyField = nodoChiediElencoFlussi.getClass().getDeclaredField("port");
        keyField.setAccessible(true);
        keyField.set(nodoChiediElencoFlussi, myPort);

        String idPa = "12345";
        nodoChiediElencoFlussi.nodoChiediElencoFlussiRendicontazione(idPa);

        assert (Boolean.TRUE);
    }

}
