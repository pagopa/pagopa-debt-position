package it.gov.pagopa.hubpa;

import static org.junit.Assert.assertTrue;

import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.queue.CloudQueueMessage;

import org.junit.ClassRule;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import it.gov.pagopa.hubpa.service.FlowsService;
import it.gov.pagopa.hubpa.servicewsdl.TipoElencoFlussiRendicontazione;
import it.gov.pagopa.hubpa.servicewsdl.TipoIdRendicontazione;

@Testcontainers
class FlowsServiceIntegrationTest {

    @ClassRule
    @Container
    public static GenericContainer<?> azurite = new GenericContainer<>(
            DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest")).withExposedPorts(10001, 10002,
                    10000);

    Logger logger = Logger.getLogger("testlogging");

    String storageConnectionString = String.format(
            "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;TableEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1",
            azurite.getContainerIpAddress(), azurite.getMappedPort(10002), azurite.getContainerIpAddress(),
            azurite.getMappedPort(10001));
    String flowsTable = "testtable";
    String flowsQueue = "testqueue";

    @Test
    void flowsBatchProcessingTest() throws ParseException, DatatypeConfigurationException, InvalidKeyException,
            URISyntaxException, StorageException, JsonProcessingException {

        CloudStorageAccount.parse(storageConnectionString).createCloudQueueClient().getQueueReference(this.flowsQueue)
                .createIfNotExists();

        CloudStorageAccount.parse(storageConnectionString).createCloudTableClient().getTableReference(this.flowsTable)
                .createIfNotExists();

        DateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
        Date date1 = format.parse("2014-04-24 11:15:00");
        GregorianCalendar cal1 = new GregorianCalendar();
        cal1.setTime(date1);

        Date date2 = format.parse("2015-04-24 11:15:00");
        GregorianCalendar cal2 = new GregorianCalendar();
        cal2.setTime(date2);

        Date date4 = format.parse("2016-04-24 11:15:00");
        GregorianCalendar cal4 = new GregorianCalendar();
        cal4.setTime(date4);

        TipoElencoFlussiRendicontazione elencoFlussi = new TipoElencoFlussiRendicontazione();
        elencoFlussi.setTotRestituiti(2);

        TipoIdRendicontazione e1 = new TipoIdRendicontazione();
        String id1 = UUID.randomUUID().toString();
        e1.setIdentificativoFlusso(id1);
        e1.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal1).toGregorianCalendar()));

        TipoIdRendicontazione e2 = new TipoIdRendicontazione();
        String id2 = UUID.randomUUID().toString();
        e2.setIdentificativoFlusso(id2);
        e2.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal2).toGregorianCalendar()));

        TipoIdRendicontazione e3 = new TipoIdRendicontazione();
        e3.setIdentificativoFlusso(id2);
        e3.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal2).toGregorianCalendar()));

        TipoIdRendicontazione e4 = new TipoIdRendicontazione();
        String id4 = UUID.randomUUID().toString();
        e4.setIdentificativoFlusso(id4);
        e4.setDataOraFlusso(DatatypeFactory.newInstance().newXMLGregorianCalendar(
                DatatypeFactory.newInstance().newXMLGregorianCalendar(cal4).toGregorianCalendar()));

        elencoFlussi.getIdRendicontazione().add(e1);
        elencoFlussi.getIdRendicontazione().add(e2);

        List<TipoIdRendicontazione> flows = elencoFlussi.getIdRendicontazione();

        FlowsService flowsService = new FlowsService(this.storageConnectionString, this.flowsTable, this.flowsQueue,
                logger);

        // flows processing
        flowsService.flowsProcessing(flows, "idPA");

        // duplicate flows processing
        flows.add(e3);
        flows.add(e3);
        flowsService.flowsProcessing(flows, "idPA");

        // single flow
        flowsService.flowProcessing(e4, "idPA");

        Iterable<CloudQueueMessage> messagges = CloudStorageAccount.parse(storageConnectionString)
                .createCloudQueueClient().getQueueReference(this.flowsQueue).retrieveMessages(32);

        List<String> ids = Arrays.asList(id1, id2, id4);

        for (CloudQueueMessage cloudQueueMessage : messagges) {

            assertTrue(cloudQueueMessage.getMessageContentAsString().contains(ids.get(0))
                    || cloudQueueMessage.getMessageContentAsString().contains(ids.get(1))
                    || cloudQueueMessage.getMessageContentAsString().contains(ids.get(2)));
        }

    }

}
