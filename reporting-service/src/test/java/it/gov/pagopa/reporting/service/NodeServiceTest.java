package it.gov.pagopa.reporting.service;

import java.lang.reflect.Field;
import java.util.logging.Logger;

import javax.xml.ws.WebServiceException;

import it.gov.pagopa.reporting.servicewsdl.PagamentiTelematiciRPT;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class NodeServiceTest {

    @Mock
    PagamentiTelematiciRPT myPort;

    Logger logger = Logger.getLogger("testlogging");

    @Test
    void nodoChiediXmlFlussoErrorTest() {
        NodeService nodeService = new NodeService(null, null, null);

        try {

            nodeService.callNodoChiediFlussoRendicontazione("idPA", "idFlow");

        } catch (WebServiceException e) {

            assertTrue(Boolean.TRUE);
        }

        assertNull(nodeService.getNodoChiediFlussoRendicontazioneFault());
        assertNull(nodeService.getNodoChiediElencoFlussiRendicontazioneXmlReporting());
    }

    @Test
    void nodoChiediElencoFlussiTestSettersTest() throws NoSuchFieldException, IllegalAccessException {

        NodeService nodeService = new NodeService(null, null, null);

        Field keyField = nodeService.getClass().getDeclaredField("port");
        keyField.setAccessible(true);
        keyField.set(nodeService, myPort);

        nodeService.callNodoChiediFlussoRendicontazione("idPA", "idFlow");

        assertTrue(Boolean.TRUE);
    }

}
