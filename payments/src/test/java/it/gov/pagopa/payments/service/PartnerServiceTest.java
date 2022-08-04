package it.gov.pagopa.payments.service;

import com.microsoft.azure.storage.CloudStorageAccount;
import com.microsoft.azure.storage.RetryNoRetry;
import com.microsoft.azure.storage.StorageException;
import com.microsoft.azure.storage.table.CloudTable;
import com.microsoft.azure.storage.table.CloudTableClient;
import com.microsoft.azure.storage.table.TableRequestOptions;
import feign.FeignException;
import feign.RetryableException;
import it.gov.pagopa.payments.endpoints.validation.PaymentValidator;
import it.gov.pagopa.payments.endpoints.validation.exceptions.PartnerValidationException;
import it.gov.pagopa.payments.mock.MockUtil;
import it.gov.pagopa.payments.mock.PaDemandNoticePaymentReqMock;
import it.gov.pagopa.payments.mock.PaGetPaymentReqMock;
import it.gov.pagopa.payments.mock.PaSendRTReqMock;
import it.gov.pagopa.payments.mock.PaVerifyPaymentNoticeReqMock;
import it.gov.pagopa.payments.model.DebtPositionStatus;
import it.gov.pagopa.payments.model.PaaErrorEnum;
import it.gov.pagopa.payments.model.PaymentOptionModel;
import it.gov.pagopa.payments.model.PaymentOptionModelResponse;
import it.gov.pagopa.payments.model.PaymentOptionStatus;
import it.gov.pagopa.payments.model.PaymentsModelResponse;
import it.gov.pagopa.payments.model.partner.ObjectFactory;
import it.gov.pagopa.payments.model.partner.PaGetPaymentReq;
import it.gov.pagopa.payments.model.partner.PaGetPaymentRes;
import it.gov.pagopa.payments.model.partner.PaSendRTReq;
import it.gov.pagopa.payments.model.partner.PaSendRTRes;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeReq;
import it.gov.pagopa.payments.model.partner.PaVerifyPaymentNoticeRes;
import it.gov.pagopa.payments.model.partner.StAmountOption;
import it.gov.pagopa.payments.model.partner.StOutcome;
import it.gov.pagopa.payments.model.spontaneous.PaymentPositionModel;
import it.gov.pagopa.payments.utils.AzuriteStorageUtil;
import lombok.extern.slf4j.Slf4j;
import org.junit.ClassRule;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import org.xml.sax.SAXException;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLStreamException;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

@Testcontainers
@ExtendWith(MockitoExtension.class)
@Slf4j
class PartnerServiceTest {

    @InjectMocks
    private PartnerService partnerService;

    @Mock
    PaymentValidator paymentValidator;

    @Mock
    private ObjectFactory factory;

    @Mock
    private GpdClient gpdClient;

    @Mock
    private GpsClient gpsClient;

    private String genericService = "/xsd/general-service.xsd";
    ResourceLoader resourceLoader = new DefaultResourceLoader();
    Resource resource = resourceLoader.getResource(genericService);

    private final ObjectFactory factoryUtil = new ObjectFactory();


    @ClassRule
    @Container
    public static GenericContainer<?> azurite =
            new GenericContainer<>(
                    DockerImageName.parse("mcr.microsoft.com/azure-storage/azurite:latest"))
                    .withExposedPorts(10001, 10002, 10000);


    String storageConnectionString =
            String.format(
                    "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;TableEndpoint=http://%s:%s/devstoreaccount1;QueueEndpoint=http://%s:%s/devstoreaccount1;BlobEndpoint=http://%s:%s/devstoreaccount1",
                    azurite.getContainerIpAddress(),
                    azurite.getMappedPort(10002),
                    azurite.getContainerIpAddress(),
                    azurite.getMappedPort(10001),
                    azurite.getContainerIpAddress(),
                    azurite.getMappedPort(10000));

    @Test
    void paVerifyPaymentNoticeTest() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

        when(factory.createPaVerifyPaymentNoticeRes()).thenReturn(factoryUtil.createPaVerifyPaymentNoticeRes());
        when(factory.createCtPaymentOptionDescriptionPA()).thenReturn(factoryUtil.createCtPaymentOptionDescriptionPA());
        when(factory.createCtPaymentOptionsDescriptionListPA())
                .thenReturn(factoryUtil.createCtPaymentOptionsDescriptionListPA());

        PaymentsModelResponse paymentModel = MockUtil.readModelFromFile("gpd/getPaymentOption.json", PaymentsModelResponse.class);
        when(gpdClient.getPaymentOption(anyString(), anyString())).thenReturn(paymentModel);

        // Test execution
        PaVerifyPaymentNoticeRes responseBody = partnerService.paVerifyPaymentNotice(requestBody);

        // Test post condition
        assertThat(responseBody.getOutcome()).isEqualTo(StOutcome.OK);
        assertThat(responseBody.getPaymentList().getPaymentOptionDescription().isAllCCP()).isFalse();
        assertThat(responseBody.getPaymentList().getPaymentOptionDescription().getAmount())
                .isEqualTo(new BigDecimal(1055));
        assertThat(responseBody.getPaymentList().getPaymentOptionDescription().getOptions())
                .isEqualTo(StAmountOption.EQ); // de-scoping
        assertThat(responseBody.getFiscalCodePA()).isEqualTo("77777777777");
        assertThat(responseBody.getPaymentDescription()).isEqualTo("string");
    }

    @Test
    void paVerifyPaymentNoticeTestKOConfig() throws DatatypeConfigurationException {

        // Test preconditions
        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

        doThrow(new PartnerValidationException(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO))
                .when(paymentValidator).isAuthorize(anyString(), anyString(), anyString());

        // Test execution
        try {

            partnerService.paVerifyPaymentNotice(requestBody);

        } catch (PartnerValidationException e) {
            // Test post condition
            assertThat(e.getError().getFaultCode()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultCode());
            assertThat(e.getError().getDescription()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getDescription());
            assertThat(e.getError().getFaultString()).isEqualTo(PaaErrorEnum.PAA_ID_INTERMEDIARIO_ERRATO.getFaultString());
        }
    }

    @Test
    void paVerifyPaymentTestKONotFound() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

        var e = Mockito.mock(FeignException.NotFound.class);
        when(gpdClient.getPaymentOption(anyString(), anyString()))
                .thenThrow(e);

        try {
            // Test execution
            partnerService.paVerifyPaymentNotice(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            assertEquals(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO, ex.getError());
        }
    }

    @Test
    void paVerifyPaymentTestKOGeneric() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

        var e = Mockito.mock(FeignException.FeignClientException.class);
        when(gpdClient.getPaymentOption(anyString(), anyString()))
                .thenThrow(e);

        try {
            // Test execution
            partnerService.paVerifyPaymentNotice(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            assertEquals(PaaErrorEnum.PAA_SYSTEM_ERROR, ex.getError());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"INVALID", "EXPIRED"})
    void paVerifyPaymentNoticeStatusKOTest(String status) throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

        PaymentsModelResponse paymentModel = MockUtil.readModelFromFile("gpd/getPaymentOption.json", PaymentsModelResponse.class);
        paymentModel.setDebtPositionStatus(DebtPositionStatus.valueOf(status));
        when(gpdClient.getPaymentOption(anyString(), anyString())).thenReturn(paymentModel);

        // Test post condition
        try {
            // Test execution
            partnerService.paVerifyPaymentNotice(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            if (DebtPositionStatus.valueOf(status).equals(DebtPositionStatus.EXPIRED)) {
                assertEquals(PaaErrorEnum.PAA_PAGAMENTO_SCADUTO, ex.getError());
            } else if (DebtPositionStatus.valueOf(status).equals(DebtPositionStatus.INVALID)) {
                assertEquals(PaaErrorEnum.PAA_PAGAMENTO_ANNULLATO, ex.getError());
            } else {
                fail();
            }
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"DRAFT", "PUBLISHED"})
    void paVerifyPaymentNoticeStatusKOTest2(String status) throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

        PaymentsModelResponse paymentModel = MockUtil.readModelFromFile("gpd/getPaymentOption.json", PaymentsModelResponse.class);
        paymentModel.setDebtPositionStatus(DebtPositionStatus.valueOf(status));
        when(gpdClient.getPaymentOption(anyString(), anyString())).thenReturn(paymentModel);

        // Test post condition
        try {
            // Test execution
            partnerService.paVerifyPaymentNotice(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            if (DebtPositionStatus.valueOf(status).equals(DebtPositionStatus.DRAFT) || DebtPositionStatus.valueOf(status).equals(DebtPositionStatus.PUBLISHED)) {
                assertEquals(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO, ex.getError());
            } else {
                fail();
            }
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"PARTIALLY_PAID", "PAID", "REPORTED"})
    void paVerifyPaymentNoticeStatusKOTest3(String status) throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaVerifyPaymentNoticeReq requestBody = PaVerifyPaymentNoticeReqMock.getMock();

        PaymentsModelResponse paymentModel = MockUtil.readModelFromFile("gpd/getPaymentOption.json", PaymentsModelResponse.class);
        paymentModel.setDebtPositionStatus(DebtPositionStatus.valueOf(status));
        when(gpdClient.getPaymentOption(anyString(), anyString())).thenReturn(paymentModel);

        // Test post condition
        try {
            // Test execution
            partnerService.paVerifyPaymentNotice(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            if (DebtPositionStatus.valueOf(status).equals(DebtPositionStatus.PARTIALLY_PAID) || DebtPositionStatus.valueOf(status).equals(DebtPositionStatus.PAID) || DebtPositionStatus.valueOf(status).equals(DebtPositionStatus.REPORTED)) {
                assertEquals(PaaErrorEnum.PAA_PAGAMENTO_DUPLICATO, ex.getError());
            } else {
                fail();
            }
        }
    }

    @Test
    void paGetPaymentTest() throws PartnerValidationException, DatatypeConfigurationException, IOException {

        // Test preconditions
        PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();

        when(factory.createPaGetPaymentRes()).thenReturn(factoryUtil.createPaGetPaymentRes());
        when(factory.createCtPaymentPA()).thenReturn(factoryUtil.createCtPaymentPA());
        when(factory.createCtSubject()).thenReturn(factoryUtil.createCtSubject());
        when(factory.createCtEntityUniqueIdentifier()).thenReturn(factoryUtil.createCtEntityUniqueIdentifier());
        when(factory.createCtTransferListPA()).thenReturn(factoryUtil.createCtTransferListPA());
        when(factory.createCtTransferPA()).thenReturn(factoryUtil.createCtTransferPA());

        when(gpdClient.getPaymentOption(anyString(), anyString()))
                .thenReturn(MockUtil.readModelFromFile("gpd/getPaymentOption.json", PaymentsModelResponse.class));

        // Test execution
        PaGetPaymentRes responseBody = partnerService.paGetPayment(requestBody);

        // Test post condition
        assertThat(responseBody.getData().getCreditorReferenceId()).isEqualTo("11111111112222222");
        assertThat(responseBody.getData().getDescription()).isEqualTo("string");
        assertThat(responseBody.getData().getDueDate())
                .isEqualTo(DatatypeFactory.newInstance().newXMLGregorianCalendar("2122-02-24T17:03:59.408"));
        assertThat(responseBody.getData().getRetentionDate())
                .isEqualTo(DatatypeFactory.newInstance().newXMLGregorianCalendar("2022-02-25T17:03:59.408"));
        assertEquals("77777777777", requestBody.getQrCode().getFiscalCode());
    }

    @Test
    void paGetPaymentTestKONotFound() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();

        var e = Mockito.mock(FeignException.NotFound.class);
        when(gpdClient.getPaymentOption(anyString(), anyString()))
                .thenThrow(e);

        try {
            // Test execution
            PaGetPaymentRes responseBody = partnerService.paGetPayment(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            assertEquals(PaaErrorEnum.PAA_PAGAMENTO_SCONOSCIUTO, ex.getError());
        }
    }

    @Test
    void paGetPaymentTestKOGeneric() throws DatatypeConfigurationException, IOException {

        // Test preconditions
        PaGetPaymentReq requestBody = PaGetPaymentReqMock.getMock();

        var e = Mockito.mock(FeignException.FeignClientException.class);
        when(gpdClient.getPaymentOption(anyString(), anyString()))
                .thenThrow(e);

        try {
            // Test execution
            PaGetPaymentRes responseBody = partnerService.paGetPayment(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            assertEquals(PaaErrorEnum.PAA_SYSTEM_ERROR, ex.getError());
        }
    }

    @Test
    void paSendRTTest() throws DatatypeConfigurationException, IOException {


        var pService = spy(new PartnerService(factory, storageConnectionString, "receiptsTable",  resource, gpdClient, gpsClient, paymentValidator));

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        doNothing().doThrow(PartnerValidationException.class).when(paymentValidator).isAuthorize(anyString(), anyString(), anyString());

        when(factory.createPaSendRTRes()).thenReturn(factoryUtil.createPaSendRTRes());

        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenReturn(MockUtil.readModelFromFile("gpd/receiptPaymentOption.json", PaymentOptionModelResponse.class));

        try {
            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
            TableRequestOptions tableRequestOptions = new TableRequestOptions();
            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance());
            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
            CloudTable table = cloudTableClient.getTableReference("receiptsTable");
            table.createIfNotExists();
        } catch (Exception e) {
            log.info("Error during table creation", e);
        }


        // Test execution
        PaSendRTRes responseBody = pService.paSendRT(requestBody);

        // Test post condition
        assertThat(responseBody.getOutcome()).isEqualTo(StOutcome.OK);
        assertThat(responseBody.getFault()).isNull();
    }

    @Test
    void paSendRTTestKOConflict() throws DatatypeConfigurationException, IOException {

        var pService = spy(new PartnerService(factory, storageConnectionString, "receiptsTable", resource, gpdClient, gpsClient, paymentValidator));

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        var e = Mockito.mock(FeignException.Conflict.class);
        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenThrow(e);

        try {
            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
            TableRequestOptions tableRequestOptions = new TableRequestOptions();
            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance());
            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
            CloudTable table = cloudTableClient.getTableReference("receiptsTable");
            table.createIfNotExists();
        } catch (Exception ex) {
            log.info("Error during table creation", e);
        }

        try {
            // Test execution
            PaSendRTRes responseBody = pService.paSendRT(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            assertEquals(PaaErrorEnum.PAA_RECEIPT_DUPLICATA, ex.getError());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"PO_UNPAID", "PO_PARTIALLY_REPORTED", "PO_REPORTED"})
    void paSendRTTestKOStatus(String status) throws DatatypeConfigurationException, IOException {

        var pService = spy(new PartnerService(factory, storageConnectionString, "receiptsTable", resource, gpdClient, gpsClient, paymentValidator));

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        PaymentOptionModelResponse paymentOption = MockUtil.readModelFromFile("gpd/receiptPaymentOption.json", PaymentOptionModelResponse.class);
        paymentOption.setStatus(PaymentOptionStatus.valueOf(status));
        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenReturn(paymentOption);

        try {
            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
            TableRequestOptions tableRequestOptions = new TableRequestOptions();
            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance());
            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
            CloudTable table = cloudTableClient.getTableReference("receiptsTable");
            table.createIfNotExists();
        } catch (Exception ex) {
            log.info("Error during table creation", ex);
        }

        try {
            // Test execution
            PaSendRTRes responseBody = pService.paSendRT(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            assertEquals(PaaErrorEnum.PAA_SEMANTICA, ex.getError());
        }
    }

    @Test
    void paSendRTTestKORetryableException() throws DatatypeConfigurationException, IOException {

        var pService = spy(new PartnerService(factory, storageConnectionString, "receiptsTable", resource, gpdClient, gpsClient, paymentValidator));

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        var e = Mockito.mock(RetryableException.class);
        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenThrow(e);

        try {
            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
            TableRequestOptions tableRequestOptions = new TableRequestOptions();
            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance());
            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
            CloudTable table = cloudTableClient.getTableReference("receiptsTable");
            table.createIfNotExists();
        } catch (Exception ex) {
            log.info("Error during table creation", e);
        }

        try {
            // Test execution
            PaSendRTRes responseBody = pService.paSendRT(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            assertEquals(PaaErrorEnum.PAA_SYSTEM_ERROR, ex.getError());
        }
    }

    @Test
    void paSendRTTestKOFeignException() throws DatatypeConfigurationException, IOException {

        var pService = spy(new PartnerService(factory, storageConnectionString, "receiptsTable", resource, gpdClient, gpsClient, paymentValidator));
        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        var e = Mockito.mock(FeignException.class);
        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenThrow(e);

        try {
            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
            TableRequestOptions tableRequestOptions = new TableRequestOptions();
            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance());
            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
            CloudTable table = cloudTableClient.getTableReference("receiptsTable");
            table.createIfNotExists();
        } catch (Exception ex) {
            log.info("Error during table creation", e);
        }

        try {
            // Test execution
            PaSendRTRes responseBody = pService.paSendRT(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            assertEquals(PaaErrorEnum.PAA_SEMANTICA, ex.getError());
        }
    }

    @Test
    void paSendRTTestKO() throws DatatypeConfigurationException, IOException {

        var pService = spy(new PartnerService(factory, storageConnectionString, "receiptsTable", resource, gpdClient, gpsClient, paymentValidator));

        // Test preconditions
        PaSendRTReq requestBody = PaSendRTReqMock.getMock();

        var e = Mockito.mock(NullPointerException.class);
        when(gpdClient.receiptPaymentOption(anyString(), anyString(), any(PaymentOptionModel.class)))
                .thenThrow(e);

        try {
            CloudStorageAccount cloudStorageAccount = CloudStorageAccount.parse(storageConnectionString);
            CloudTableClient cloudTableClient = cloudStorageAccount.createCloudTableClient();
            TableRequestOptions tableRequestOptions = new TableRequestOptions();
            tableRequestOptions.setRetryPolicyFactory(RetryNoRetry.getInstance());
            cloudTableClient.setDefaultRequestOptions(tableRequestOptions);
            CloudTable table = cloudTableClient.getTableReference("receiptsTable");
            table.createIfNotExists();
        } catch (Exception ex) {
            log.info("Error during table creation", e);
        }

        try {
            // Test execution
            PaSendRTRes responseBody = pService.paSendRT(requestBody);
            fail();
        } catch (PartnerValidationException ex) {
            // Test post condition
            assertEquals(PaaErrorEnum.PAA_SYSTEM_ERROR, ex.getError());
        }
    }

    @Test
    void azureStorageTest() throws InvalidKeyException, URISyntaxException, StorageException {
        AzuriteStorageUtil azuriteStorageUtil = new AzuriteStorageUtil(storageConnectionString, true);
        azuriteStorageUtil.createTable("testTable");
        // se arrivo a questa riga la tabella è stata creata
        assertTrue(true);

    }


    @Test
    void paDemandPaymentNoticeTest() throws DatatypeConfigurationException, IOException, XMLStreamException, ParserConfigurationException, SAXException {
        var pService = spy(new PartnerService(factory, storageConnectionString, "receiptsTable", resource, gpdClient, gpsClient, paymentValidator));

        // Test preconditions
        var requestBody = PaDemandNoticePaymentReqMock.getMock();

        when(factory.createPaDemandPaymentNoticeResponse()).thenReturn(factoryUtil.createPaDemandPaymentNoticeResponse());
        when(factory.createCtQrCode()).thenReturn(factoryUtil.createCtQrCode());
        when(factory.createCtPaymentOptionsDescriptionListPA()).thenReturn(factoryUtil.createCtPaymentOptionsDescriptionListPA());
        when(factory.createCtPaymentOptionDescriptionPA()).thenReturn(factoryUtil.createCtPaymentOptionDescriptionPA());

        var paymentModel = MockUtil.readModelFromFile("gps/createSpontaneousPayments.json", PaymentPositionModel.class);
        when(gpsClient.createSpontaneousPayments(anyString(), any())).thenReturn(paymentModel);

        // Test execution
        var responseBody = pService.paDemandPaymentNotice(requestBody);

        // Test post condition
        assertThat(responseBody.getOutcome()).isEqualTo(StOutcome.OK);
        assertThat(responseBody.getPaymentList().getPaymentOptionDescription().isAllCCP()).isFalse();
        assertThat(responseBody.getPaymentList().getPaymentOptionDescription().getAmount())
                .isEqualTo(new BigDecimal(1055));
        assertThat(responseBody.getPaymentList().getPaymentOptionDescription().getOptions())
                .isEqualTo(StAmountOption.EQ); // de-scoping
        assertThat(responseBody.getFiscalCodePA()).isEqualTo("77777777777");
        assertThat(responseBody.getPaymentDescription()).isEqualTo("string");
    }
}
