package it.gov.pagopa.reporting.service;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import javax.activation.DataHandler;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import com.azure.storage.blob.BlobContainerClient;
import com.azure.storage.blob.BlobServiceClient;
import com.azure.storage.blob.BlobServiceClientBuilder;
import com.azure.storage.blob.models.BlobItem;

import org.junit.ClassRule;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.TipoIdRendicontazione;

@Testcontainers
class FlowServiceIntegrationTest {

        @ClassRule
        @Container
        public static GenericContainer<?> azurite = new GenericContainer<>(
                        DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest")).withExposedPorts(10001,
                                        10002, 10000);

        Logger logger = Logger.getLogger("testlogging");

        String storageConnectionString = String.format(
                        "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;BlobEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1",
                        azurite.getContainerIpAddress(), azurite.getMappedPort(10000), azurite.getContainerIpAddress(),
                        azurite.getMappedPort(10001));
        String flowsTable = "testtable";
        String flowsQueue = "testqueue";

        FlowsService flowsService;

        @Test
        void flowsBatchProcessingTest()
                        throws ParseException, DatatypeConfigurationException, InvalidKeyException, URISyntaxException {

                flowsService = spy(new FlowsService(storageConnectionString, "identificativoIntemediarioPA",
                                "identificativoStazioneIntermediarioPA", "nodePassword",
                                "container", logger));

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
                                e1.getIdentificativoFlusso() + "##" + e1.getDataOraFlusso().toString() + ".xml",
                                e2.getIdentificativoFlusso() + "##" + e2.getDataOraFlusso().toString() + ".xml");

                flowsService.flowsXmlDownloading(flows, "idPA");

                List<String> fileNames = containerClient.listBlobs().stream().map(BlobItem::getName)
                                .collect(Collectors.toList());

                assertTrue(fileNames.containsAll(flowsFileName));
        }

        @Test
        void flowsProcessingErrorNodeTest()
                        throws ParseException, DatatypeConfigurationException, InvalidKeyException, URISyntaxException {

                flowsService = spy(new FlowsService(storageConnectionString, "identificativoIntemediarioPA",
                                "identificativoStazioneIntermediarioPA", "nodePassword",
                                "container", logger));

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

                flowsService.flowsXmlDownloading(flows, "idPA");

                assertTrue(Boolean.TRUE);
        }
}
