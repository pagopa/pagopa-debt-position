package it.gov.pagopa.reporting.service;

import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.blob.models.BlobItem;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.TableOperation;
import com.microsoft.azure.storage.table.TableResult;
import com.sun.xml.ws.client.ClientTransportException;
import it.gov.pagopa.reporting.entities.FlowEntity;
import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;
import org.junit.ClassRule;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import javax.activation.DataHandler;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import java.io.IOException;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

@Testcontainers
class FlowServiceIntegrationTest {

    @ClassRule
    @Container
    public static GenericContainer<?> azurite = new GenericContainer<>(
            DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest")).withExposedPorts(10000, 10001, 10002);

    Logger logger = Logger.getLogger("testlogging");

    String storageConnectionString = String.format(
            "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;" +
                    "AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;" +
                    "BlobEndpoint=http://%s:%s/devstoreaccount1;" +
                    "QueueEndpoint=http://%s:%s/devstoreaccount1;" +
                    "TableEndpoint=http://%s:%s/devstoreaccount1",
            azurite.getContainerIpAddress(),
            azurite.getMappedPort(10000),
            azurite.getContainerIpAddress(),
            azurite.getMappedPort(10001),
            azurite.getContainerIpAddress(),
            azurite.getMappedPort(10002)
    );
    String flowsTable = "testtable";
    String flowsQueue = "testqueue";

    FlowsService flowsService;

    @Test
    void flowsBatchProcessingTest()
            throws ParseException, DatatypeConfigurationException, InvalidKeyException, URISyntaxException {

        flowsService = spy(new FlowsService(storageConnectionString, "identificativoIntemediarioPA",
                "identificativoStazioneIntermediarioPA", "nodePassword",
                "container", "queue", "flows", 1, 60, 0, logger));

        NodeService nodeService = mock(NodeService.class);
        doReturn(mock(DataHandler.class)).when(nodeService)
                .getNodoChiediElencoFlussiRendicontazioneXmlReporting();

        doReturn(nodeService).when(flowsService).getNodeServiceInstance();

        BlobServiceClient blobServiceClient = new BlobServiceClientBuilder()
                .connectionString(this.storageConnectionString).buildClient();

        BlobContainerClient containerClient = blobServiceClient.createBlobContainer("container");

        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
        Date date1 = format.parse("2014-04-24 11:15:00");
        GregorianCalendar cal1 = new GregorianCalendar();
        cal1.setTime(date1);
        TipoIdRendicontazione e1 = new TipoIdRendicontazione();
        String id1 = UUID.randomUUID().toString();
        e1.setIdentificativoFlusso(id1);
        e1.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        TipoIdRendicontazione e2 = new TipoIdRendicontazione();
        String id2 = UUID.randomUUID().toString();
        e2.setIdentificativoFlusso(id2);
        e2.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        List<TipoIdRendicontazione> flows = Arrays.asList(e1, e2);

        List<String> flowsFileName = Arrays.asList(
                e1.getDataOraFlusso().toString().split("\\.")[0] + "##" + "idPA" + "##" + e1.getIdentificativoFlusso() + ".xml",
                e2.getDataOraFlusso().toString().split("\\.")[0] + "##" + "idPA" + "##" + e2.getIdentificativoFlusso() + ".xml");

        flowsService.flowsXmlDownloading(flows, "idPA", 0);

        List<String> fileNames = containerClient.listBlobs().stream().map(BlobItem::getName)
                .collect(Collectors.toList());

        assertTrue(fileNames.containsAll(flowsFileName));
    }

    @Test
    void flowsProcessingErrorNodeTest()
            throws ParseException, DatatypeConfigurationException, InvalidKeyException, URISyntaxException {

        flowsService = spy(new FlowsService(storageConnectionString, "identificativoIntemediarioPA",
                "identificativoStazioneIntermediarioPA", "nodePassword",
                "container", "queue", "flows", 1, 60, 0, logger));

        NodeService nodeService = mock(NodeService.class);
        doReturn(mock(FaultBean.class)).when(nodeService).getNodoChiediFlussoRendicontazioneFault();

        doReturn(nodeService).when(flowsService).getNodeServiceInstance();

        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
        Date date1 = format.parse("2014-04-24 11:15:00");
        GregorianCalendar cal1 = new GregorianCalendar();
        cal1.setTime(date1);
        TipoIdRendicontazione e1 = new TipoIdRendicontazione();
        String id1 = UUID.randomUUID().toString();
        e1.setIdentificativoFlusso(id1);
        e1.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        TipoIdRendicontazione e2 = new TipoIdRendicontazione();
        String id2 = UUID.randomUUID().toString();
        e2.setIdentificativoFlusso(id2);
        e2.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        List<TipoIdRendicontazione> flows = Arrays.asList(e1, e2);

        flowsService.flowsXmlDownloading(flows, "idPA", 0);

        assertTrue(Boolean.TRUE);
    }

    @ParameterizedTest
    @ValueSource(ints = {0, 1})
    void flowsProcessingNodeKOTest(int maxRetry)
            throws ParseException, DatatypeConfigurationException, InvalidKeyException, URISyntaxException, StorageException, JsonProcessingException {

        flowsService = spy(new FlowsService(storageConnectionString, "identificativoIntemediarioPA",
                "identificativoStazioneIntermediarioPA", "nodePassword",
                "container", "queue", "flows", maxRetry, 60, 0, logger));

        NodeService nodeService = mock(NodeService.class);

        doThrow(ClientTransportException.class).when(nodeService).callNodoChiediFlussoRendicontazione(anyString(), anyString());

        doReturn(nodeService).when(flowsService).getNodeServiceInstance();

        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
        Date date1 = format.parse("2014-04-24 11:15:00");
        GregorianCalendar cal1 = new GregorianCalendar();
        cal1.setTime(date1);
        TipoIdRendicontazione e1 = new TipoIdRendicontazione();
        String id1 = UUID.randomUUID().toString();
        e1.setIdentificativoFlusso(id1);
        e1.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        TipoIdRendicontazione e2 = new TipoIdRendicontazione();
        String id2 = UUID.randomUUID().toString();
        e2.setIdentificativoFlusso(id2);
        e2.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        List<TipoIdRendicontazione> flows = Arrays.asList(e1, e2);

        flowsService.flowsXmlDownloading(flows, "idPA", 0);

        int times = maxRetry == 1 ? 2 : 0;
        verify(flowsService, times(times)).reQueuingMessage(anyString(), any(), anyInt());
    }

    @Test
    void flowsProcessingNodeKOTest_2()
            throws ParseException, DatatypeConfigurationException, InvalidKeyException, URISyntaxException, StorageException, IOException {

        flowsService = spy(new FlowsService(storageConnectionString, "identificativoIntemediarioPA",
                "identificativoStazioneIntermediarioPA", "nodePassword",
                "container", "queue", "flows",1, 60, 0, logger));

        NodeService nodeService = mock(NodeService.class);
        doReturn(mock(DataHandler.class)).when(nodeService).getNodoChiediElencoFlussiRendicontazioneXmlReporting();
        doThrow(IOException.class).when(flowsService).saveBlob(any(), anyString(), any());

        doReturn(nodeService).when(flowsService).getNodeServiceInstance();

        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
        Date date1 = format.parse("2014-04-24 11:15:00");
        GregorianCalendar cal1 = new GregorianCalendar();
        cal1.setTime(date1);
        TipoIdRendicontazione e1 = new TipoIdRendicontazione();
        String id1 = UUID.randomUUID().toString();
        e1.setIdentificativoFlusso(id1);
        e1.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        TipoIdRendicontazione e2 = new TipoIdRendicontazione();
        String id2 = UUID.randomUUID().toString();
        e2.setIdentificativoFlusso(id2);
        e2.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        List<TipoIdRendicontazione> flows = Arrays.asList(e1, e2);

        flowsService.flowsXmlDownloading(flows, "idPA", 0);

        verify(flowsService, times(0)).reQueuingMessage(anyString(), any(), anyInt());
    }

    @Test
    void flowsProcessingNodeKOTest_3()
            throws ParseException, DatatypeConfigurationException, InvalidKeyException, URISyntaxException, StorageException, JsonProcessingException {

        flowsService = spy(new FlowsService(storageConnectionString, "identificativoIntemediarioPA",
                "identificativoStazioneIntermediarioPA", "nodePassword",
                "container", flowsQueue, flowsTable, 1, 60, 0, logger));

        NodeService nodeService = mock(NodeService.class);

        doThrow(ClientTransportException.class).when(nodeService).callNodoChiediFlussoRendicontazione(anyString(), anyString());

        doReturn(nodeService).when(flowsService).getNodeServiceInstance();

        String idPA = "77777777777";
        // flow definition
        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
        Date date1 = format.parse("2014-04-24 11:15:00");
        GregorianCalendar cal1 = new GregorianCalendar();
        cal1.setTime(date1);
        TipoIdRendicontazione flow = new TipoIdRendicontazione();
        String id1 = UUID.randomUUID().toString();
        flow.setIdentificativoFlusso(id1);
        flow.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        // add flow in FlowsTable
        CloudTable table = CloudStorageAccount.parse(storageConnectionString).createCloudTableClient()
                .getTableReference(this.flowsTable);

        table.createIfNotExists();

        table.execute(TableOperation.insert(new FlowEntity(flow.getIdentificativoFlusso(), flow.getDataOraFlusso().toString(), idPA)));

        List<TipoIdRendicontazione> flows = Arrays.asList(flow);

        flowsService.flowsXmlDownloading(flows, idPA, 1);

        TableResult count = table.execute(TableOperation.retrieve(idPA, flow.getIdentificativoFlusso(), FlowEntity.class));
        
        assertNull(count.getResult());
    }

}
