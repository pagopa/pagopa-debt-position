package it.gov.pagopa.debtposition.controller;

import static it.gov.pagopa.debtposition.controller.DebtPositionControllerV3Test.createPaymentPositionV3;
import static org.hamcrest.CoreMatchers.containsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.client.NodeClient;
import it.gov.pagopa.debtposition.dto.*;
import it.gov.pagopa.debtposition.mock.DebtPositionMock;
import it.gov.pagopa.debtposition.model.checkposition.NodeCheckPositionModel;
import it.gov.pagopa.debtposition.model.checkposition.response.NodeCheckPositionResponse;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.UpdateTransferIbanMassiveModel;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import it.gov.pagopa.debtposition.service.pd.crud.PaymentPositionCRUDService;
import it.gov.pagopa.debtposition.util.CommonUtil;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Random;
import org.hamcrest.Matchers;
import org.hamcrest.core.IsNull;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.bean.override.mockito.MockitoSpyBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class DebtPositionControllerTest {
  private static final String ORG_FISCAL_CODE = "7777777777";

  @Autowired private MockMvc mvc;

  @MockitoSpyBean private PaymentPositionCRUDService paymentPositionService;

  @Mock private ModelMapper modelMapperMock;

  @MockitoBean private NodeClient nodeClient;

  @Value("${nav.aux.digit}")
  private String auxDigit;

  @BeforeEach
  void setUp() {}

  /** CREATE DEBT POSITION */
  @Test
  void createDebtPosition_201() throws Exception {
    // a debtor position is created with null validationDate and without forcing toPublish
    mvc.perform(
            post("/organizations/123456789010/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].companyName")
                .value("mock company name"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.nullValue()));
  }

  @Test
  void createDebtPosition_blank_input_201() throws Exception {
    PaymentPositionDTO mock1 = DebtPositionMock.getMock1();
    mock1.setFullName(" ");
    mock1.setFiscalCode(" ");
    mvc.perform(
            post("/organizations/blank_12345678901/debtpositions")
                .content(TestUtil.toJson(mock1))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"));
  }

  @Test
  void createDebtPosition_type_ACA_201() throws Exception {
    mvc.perform(
            post("/organizations/aca_12345678901/debtpositions?serviceType=ACA")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"))
        // il serviceType non deve essere restituito nella risposta
        .andExpect(MockMvcResultMatchers.jsonPath("$.serviceType").doesNotExist());
  }

  @Test
  void createDebtPosition_null_input_400() throws Exception {
    PaymentPositionDTO mock1 = DebtPositionMock.getMock1();
    mock1.setFullName(null);
    mock1.setFiscalCode(null);
    mvc.perform(
            post("/organizations/12345678901/debtpositions")
                .content(TestUtil.toJson(mock1))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createDebtPositionWithStamp_201() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    TransferDTO t =
        new TransferDTO(
            pp.getOrganizationFiscalCode(),
            "1",
            pp.getPaymentOption().get(0).getAmount(),
            "info",
            "0",
            null,
            null,
            new Stamp("hash1", "01", "ML"),
            TransferStatus.T_UNREPORTED);
    pp.getPaymentOption().get(0).getTransfer().set(0, t);
    pp.setIupd(CommonUtil.randomDigits(20));
    pp.getPaymentOption().get(0).setIuv(CommonUtil.randomDigits(17));
    mvc.perform(
            post("/organizations/12345678901/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.hashDocument")
                .value(t.getStamp().getHashDocument()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.stampType")
                .value(t.getStamp().getStampType()))
        .andExpect(
            MockMvcResultMatchers.jsonPath(
                    "$.paymentOption[0].transfer[0].stamp.provincialResidence")
                .value(t.getStamp().getProvincialResidence()));
  }

  @Test
  void createDebtPosition_Multiple_201() throws Exception {
    // creazione di due posizione debitorie per la stessa organizzazione (2 PaymentOption)
    mvc.perform(
            post("/organizations/12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock2()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[1].nav")
                .value("31234562"));

    mvc.perform(
            post("/organizations/12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234563"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[1].nav")
                .value("31234564"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[2].nav")
                .value("31234565"));
  }

  @Test
  void createDebtPosition_Custom_NAV_201() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    pp.getPaymentOption().forEach(po -> po.setNav("CUSTOM_" + auxDigit + po.getIuv()));

    mvc.perform(
            post("/organizations/12345678901_NAV/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("CUSTOM_" + auxDigit + "1234561"));
  }

  @Test
  void createDebtPosition_transferToDifferentCI_200() throws Exception {

    PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
    paymentPositionDTO
        .getPaymentOption()
        .get(0)
        .getTransfer()
        .get(0)
        .setOrganizationFiscalCode("ANOTHERCI123");

    mvc.perform(
            post("/organizations/12345678901200_transferToDifferentCI/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // recupero la posizione debitoria e controllo i valori dei fiscal code degli EC
    String url =
        "/organizations/12345678901200_transferToDifferentCI/debtpositions/12345678901IUPDMOCK1";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.organizationFiscalCode")
                .value("12345678901200_transferToDifferentCI"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].organizationFiscalCode")
                .value("12345678901200_transferToDifferentCI"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].organizationFiscalCode")
                .value("ANOTHERCI123"));
  }

  @Test
  void createDebtPosition_400() throws Exception {
    mvc.perform(
            post("/organizations/400_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));

    mvc.perform(
            post("/organizations/400_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock2()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createDebtPosition_Country_400() throws Exception {
    // provo a creare una posizione debitoria dove il country non rispetti la regexp [A-Z]{2}
    mvc.perform(
            post("/organizations/400_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock9()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createDebtPosition_Amount_400() throws Exception {
    // provo a creare una posizione debitoria dove l'amount previsto per la PO differisce da quello
    // del transfer
    mvc.perform(
            post("/organizations/400_Amount_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createDebtPosition_Retention_400() throws Exception {
    // provo a creare una posizione debitoria dove la retention date è minore della due_date
    mvc.perform(
            post("/organizations/400_Retention_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock5()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createDebtPosition_Validity_400() throws Exception {
    // provo a creare una posizione debitoria dove la retention date è minore della due_date
    mvc.perform(
            post("/organizations/400_Validity_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock6()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createDebtPosition_Num_Transfers_400() throws Exception {
    // provo a creare una posizione debitoria dove il numero di trasfers supera il massimo di 5
    mvc.perform(
            post("/organizations/400_Num_Transfers_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock7()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createDebtPosition_ID_Transfer_400() throws Exception {
    // provo a creare una posizione debitoria dove l'id_transfer non è compreso tra 1 e 5
    mvc.perform(
            post("/organizations/400_ID_Transfer_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock8()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void multiInstallmentsDebtPositionNotReadableV1() throws Exception {
      String uri = String.format("/organizations/%s/debtpositions", ORG_FISCAL_CODE);
      PaymentPositionModelV3 paymentPositionV3 = createPaymentPositionV3(2, 2);
      mvc.perform(
              post("/v3" + uri)
                      .content(TestUtil.toJson(paymentPositionV3))
                      .contentType(MediaType.APPLICATION_JSON))
              .andExpect(status().isCreated());

      mvc.perform(
              get(uri + "/" + paymentPositionV3.getIupd()))
              .andExpect(status().isUnprocessableEntity());
  }

  @Test
  void multiInstallmentDebtPositionsFilteredOut() throws Exception {
      String uri = String.format("/organizations/%s/debtpositions", ORG_FISCAL_CODE);
      PaymentPositionModelV3 paymentPositionV3 = createPaymentPositionV3(2, 2);
      mvc.perform(
              post("/v3" + uri)
                      .content(TestUtil.toJson(paymentPositionV3))
                      .contentType(MediaType.APPLICATION_JSON))
              .andExpect(status().isCreated());

      mvc.perform(get(uri)
                      .param("limit", "1")
                      .param("page", "0"))
              .andExpect(status().isOk())
              .andExpect(content().contentType(MediaType.APPLICATION_JSON))
              .andExpect(jsonPath(
                      "$.payment_position_list[?(@.iupd == '%s')]", paymentPositionV3.getIupd()
              ).doesNotExist());
  }

  @Test
  void createDebtPosition_409() throws Exception {
    mvc.perform(
            post("/organizations/409_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));

    // provo a creare 2 posizioni debitorie con lo stesso organization_fiscal_code
    // => la seconda chiamata deve andare in errore con codice 409
    mvc.perform(
            post("/organizations/409_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createDebtPosition_Custom_NAV_409() throws Exception {
    PaymentPositionDTO ppNav = DebtPositionMock.getMock1();
    ppNav.getPaymentOption().forEach(po -> po.setNav("CUSTOM_" + auxDigit + po.getIuv()));

    mvc.perform(
            post("/organizations/409_12345678901_NAV/debtpositions")
                .content(TestUtil.toJson(ppNav))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("CUSTOM_" + auxDigit + "1234561"));

    // provo a creare una seconda posizione debitoria per la stessa organizzazione, cambiando lo
    // IUPD e lo IUV ma non il NAV
    // => la chiamata deve andare in errore con codice 409 (violazione unique constraint)
    ppNav.setIupd((int) (new Random().nextInt(100)) + "_" + ppNav.getIupd());
    ppNav
        .getPaymentOption()
        .forEach(po -> po.setIuv((int) (new Random().nextInt(100)) + "_" + po.getIuv()));
    mvc.perform(
            post("/organizations/409_12345678901_NAV/debtpositions")
                .content(TestUtil.toJson(ppNav))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createAndPublishDebtPosition_201() throws Exception {
    // Creo e pubblico la posizione debitoria
    mvc.perform(
            post("/organizations/CRTPUB_12345678901/debtpositions?toPublish=True")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));

    // verifico che lo stato sia stato settato a valid
    mvc.perform(
            get("/organizations/CRTPUB_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty())
        .andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.notNullValue()));

    // provo a fare una nuova pubblicazione su una posizione debitoria con uno stato non più idoneo
    mvc.perform(
            post("/organizations/CRTPUB_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void createDebtPositionAuthorizedBySegregationCodes_201() throws Exception {
    PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
    String validSegregationCode =
        paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0, 2);
    String anotherSegregationCode = "99";
    mvc.perform(
            post("/organizations/SC_12345678901/debtpositions?segregationCodes="
                    + validSegregationCode
                    + ","
                    + anotherSegregationCode)
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("31234561"));
  }

  @Test
  void createDebtPositionAuthorizedBySegregationCodes_403() throws Exception {
    String notSufficientSegregationCode = "99";
    mvc.perform(
            post("/organizations/12345678901/debtpositions?segregationCodes="
                    + notSufficientSegregationCode)
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isForbidden());
  }

  @Test
  void createDebtPositionWithMetadata_201() throws Exception {
    // creo una posizione debitoria con metadati su PO e transfer
    mvc.perform(
            post("/organizations/12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMetadataMock8()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234569"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));
  }

  @Test
  void createDebtPositionWithMetadata_400() throws Exception {
    // creo una posizione debitoria con più di 10 occorrenze di metadati (numero massimo accettato)
    // --> request rifiutata
    PaymentPositionDTO pp = DebtPositionMock.getMetadataMock8();
    PaymentOptionDTO po = pp.getPaymentOption().get(0);
    for (int i = 0; i <= 10; i++) {
      po.addPaymentOptionMetadata(
          PaymentOptionMetadataDTO.builder().key("key" + i).value("value" + i).build());
    }
    System.out.println(TestUtil.toJson(pp));
    mvc.perform(
            post("/organizations/12345678901/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().string(containsString("size must be between 0 and 10")));
  }
  
  
  @Test
  void createDebtPositionWithCheckOnIBAN_400() throws Exception {

	  // Blank postalIban
	  PaymentPositionDTO pp = DebtPositionMock.getMock1();
	  pp.getPaymentOption().get(0).getTransfer().get(0).setPostalIban("");
	  mvc.perform(
			  post("/organizations/CHKIBAN_12345678901/debtpositions")
			  .content(TestUtil.toJson(pp))
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isBadRequest())
	  .andExpect(content().contentType(MediaType.APPLICATION_JSON))
	  .andExpect(jsonPath("$.detail").value(containsString(
			  "postalIban: Postal IBAN is optional, but if provided, it must not be blank and must not exceed 35 characters"
			  )));

	  // postalIban with single space
	  pp.getPaymentOption().get(0).getTransfer().get(0).setPostalIban(" ");
	  mvc.perform(
			  post("/organizations/CHKIBAN_12345678901/debtpositions")
			  .content(TestUtil.toJson(pp))
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON))
	  .andExpect(jsonPath("$.detail").value(containsString(
			  "postalIban: Postal IBAN must not contain spaces or special characters"
			  )));

	  // postalIban with special characters
	  pp = DebtPositionMock.getMock1();
	  pp.getPaymentOption().get(0).getTransfer().get(0).setPostalIban("IT60X054281110!@#0000123456");
	  mvc.perform(post("/organizations/CHKIBAN_12345678901/debtpositions")
			  .content(TestUtil.toJson(pp))
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isBadRequest()).andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON))
	  .andExpect(jsonPath("$.detail").value(containsString(
			  "postalIban: Postal IBAN must not contain spaces or special characters"
			  )));

	  // postalIban too long
	  pp = DebtPositionMock.getMock1();
	  pp.getPaymentOption().get(0).getTransfer().get(0).setPostalIban("IT60X054281110100000012345678901234567"); // 36 chars
	  mvc.perform(post("/organizations/CHKIBAN_12345678901/debtpositions")
			  .content(TestUtil.toJson(pp))
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isBadRequest())
	  .andExpect(content().contentType(MediaType.APPLICATION_JSON))
	  .andExpect(jsonPath("$.detail").value(containsString(
			  "postalIban: Postal IBAN is optional, but if provided, it must not be blank and must not exceed 35 characters"
			  )));

	  // iban with spaces
	  pp = DebtPositionMock.getMock1();
	  pp.getPaymentOption().get(0).getTransfer().get(0).setIban("IT60 X0542811101000000123456");
	  mvc.perform(post("/organizations/CHKIBAN_12345678901/debtpositions")
			  .content(TestUtil.toJson(pp))
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isBadRequest())
	  .andExpect(status().isBadRequest()).andExpect(content().contentType(MediaType.APPLICATION_JSON))
	  .andExpect(jsonPath("$.detail").value(containsString(
			  "iban: IBAN must not contain spaces or special characters"
			  )));

	  // iban too long (36 characters)
	  pp = DebtPositionMock.getMock1();
	  pp.getPaymentOption().get(0).getTransfer().get(0).setIban("IT60X054281110100000012345678901234567");
	  mvc.perform(post("/organizations/CHKIBAN_12345678901/debtpositions")
			  .content(TestUtil.toJson(pp))
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isBadRequest())
	  .andExpect(content().contentType(MediaType.APPLICATION_JSON))
	  .andExpect(jsonPath("$.detail").value(containsString(
			  "iban: The IBAN must not be blank and must not exceed 35 characters"
			  )));
  }
  

  /** GET DEBT POSITION BY IUV */
  @Test
  void getDebtPositionByIUV_200() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    pp.setIupd(CommonUtil.randomDigits(20));
    String iuv = CommonUtil.randomDigits(17);
    pp.getPaymentOption().get(0).setIuv(iuv);
    mvc.perform(
            post("/organizations/20077777771/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String url = "/organizations/20077777771/paymentoptions/" + iuv + "/debtposition";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value(auxDigit + iuv));
  }

  @Test
  void getDebtPositionWithStampByIUV_200() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    TransferDTO t =
        new TransferDTO(
            pp.getOrganizationFiscalCode(),
            "1",
            pp.getPaymentOption().get(0).getAmount(),
            "info",
            "0",
            null,
            null,
            new Stamp("hash1", "01", "ML"),
            TransferStatus.T_UNREPORTED);
    pp.getPaymentOption().get(0).getTransfer().set(0, t);
    String iuv = CommonUtil.randomDigits(17);
    pp.getPaymentOption().get(0).setIuv(iuv);
    pp.setIupd(CommonUtil.randomDigits(20));
    mvc.perform(
            post("/organizations/20077777771/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());
    String url = "/organizations/20077777771/paymentoptions/" + iuv + "/debtposition";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.hashDocument")
                .value(t.getStamp().getHashDocument()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.stampType")
                .value(t.getStamp().getStampType()))
        .andExpect(
            MockMvcResultMatchers.jsonPath(
                    "$.paymentOption[0].transfer[0].stamp.provincialResidence")
                .value(t.getStamp().getProvincialResidence()));
  }

  @Test
  void getDebtPositionByIUV_WithMetadata_200() throws Exception {
    // Creo una posizione debitoria con metadati su PO e transfer, la recupero e verifico siano
    // presenti i metadati inseriti
    PaymentPositionDTO pp = DebtPositionMock.getMetadataMock8();
    String iuv = "47999999999999999";
    pp.getPaymentOption().get(0).setIuv(iuv);
    mvc.perform(
            post("/organizations/20077777772/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value(auxDigit + iuv))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));

    String url = "/organizations/20077777772/paymentoptions/" + iuv + "/debtposition";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value(auxDigit + iuv))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));
  }

  @Test
  void getDebtPositionByIUV_Custom_NAV_200() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    String iuv = "47999999999999999";
    pp.getPaymentOption().get(0).setIuv(iuv);
    pp.getPaymentOption().forEach(po -> po.setNav("9" + auxDigit + po.getIuv()));

    // Creo una posizione debitoria settando il NAV e la recupero
    mvc.perform(
            post("/organizations/20077777773/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("9" + auxDigit + iuv));
    String url = "/organizations/20077777773/paymentoptions/" + iuv + "/debtposition";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("9" + auxDigit + iuv));
  }

  @Test
  void getDebtPositionByIUV_SegregationCodeAuthorized_200() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    String iuv = "47999999999999999";
    pp.getPaymentOption().get(0).setIuv(iuv);
    String validSegregationCode = pp.getPaymentOption().get(0).getIuv().substring(0, 2);
    String anotherSegregationCode = "99";
    // creo una posizione debitoria e la recupero
    mvc.perform(
            post("/organizations/20077777774/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String url =
        "/organizations/20077777774/paymentoptions/"
            + iuv
            + "/debtposition?segregationCodes="
            + validSegregationCode
            + ","
            + anotherSegregationCode;
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value(auxDigit + iuv));
  }

  @Test
  void getDebtPositionByIUV_SegregationCodeForbidden_403() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    String iuv = "47999999999999999";
    pp.getPaymentOption().get(0).setIuv(iuv);
    String notSufficientSegregationCode = "99";
    mvc.perform(
            post("/organizations/40377777771/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());
    String url =
        "/organizations/40377777771/paymentoptions/"
            + iuv
            + "/debtposition?segregationCodes="
            + notSufficientSegregationCode;
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isForbidden());
  }

  @Test
  void getDebtPositionByIUV_404() throws Exception {
    String NOT_EXISTENT_IUV = "00999999999999999";
    String url = "/organizations/40377777771/paymentoptions/" + NOT_EXISTENT_IUV + "/debtposition";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  /** GET DEBT POSITION BY IUPD */
  @Test
  void getDebtPositionByIUPD_200() throws Exception {
    // creo una posizione debitoria e la recupero
    mvc.perform(
            post("/organizations/200_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String url = "/organizations/200_12345678901/debtpositions/12345678901IUPDMOCK1";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].companyName")
                .value("mock company name"));
  }

  @Test
  void getDebtPositionWithStampByIUPD_200() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    TransferDTO t =
        new TransferDTO(
            pp.getOrganizationFiscalCode(),
            "1",
            pp.getPaymentOption().get(0).getAmount(),
            "info",
            "0",
            null,
            null,
            new Stamp("hash1", "01", "ML"),
            TransferStatus.T_UNREPORTED);
    pp.getPaymentOption().get(0).getTransfer().set(0, t);
    mvc.perform(
            post("/organizations/20077777771/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String url = "/organizations/20077777771/debtpositions/" + pp.getIupd();
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.hashDocument")
                .value(t.getStamp().getHashDocument()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].stamp.stampType")
                .value(t.getStamp().getStampType()))
        .andExpect(
            MockMvcResultMatchers.jsonPath(
                    "$.paymentOption[0].transfer[0].stamp.provincialResidence")
                .value(t.getStamp().getProvincialResidence()));
  }

  @Test
  void getDebtPositionByIUPDWithMetadata_200() throws Exception {
    // creo una posizione debitoria con metadati su PO e transfer, la recupero e verifico siano
    // presenti i metadati inseriti
    mvc.perform(
            post("/organizations/200_metadata_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMetadataMock8()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234569"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));

    String url =
        "/organizations/200_metadata_12345678901/debtpositions/12345678901IUPDMETADATAMOCK7";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234569"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));
  }

  @Test
  void getDebtPositionByIUPD_Custom_NAV_200() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    pp.getPaymentOption().forEach(po -> po.setNav("CUSTOM_" + auxDigit + po.getIuv()));

    // creo una posizione debitoria settando il NAV e la recupero
    mvc.perform(
            post("/organizations/200_12345678901_NAV/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("CUSTOM_" + auxDigit + "1234561"));

    String url = "/organizations/200_12345678901_NAV/debtpositions/12345678901IUPDMOCK1";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("CUSTOM_" + auxDigit + "1234561"));
  }

  @Test
  void getDebtPositionByIUPD_SegregationCodeAuthorized_200() throws Exception {
    PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
    String validSegregationCode =
        paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0, 2);
    String anotherSegregationCode = "99";
    // creo una posizione debitoria e la recupero
    mvc.perform(
            post("/organizations/200_SC_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String url =
        "/organizations/200_SC_12345678901/debtpositions/12345678901IUPDMOCK1?segregationCodes="
            + validSegregationCode
            + ","
            + anotherSegregationCode;
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value("31234561"));
  }

  @Test
  void getDebtPositionByIUPD_SegregationCodeForbidden_403() throws Exception {
    String notSufficientSegregationCode = "99";
    // creo una posizione debitoria e la recupero
    mvc.perform(
            post("/organizations/403_SC_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String url =
        "/organizations/200_SC_12345678901/debtpositions/12345678901IUPDMOCK1?segregationCodes="
            + notSufficientSegregationCode;
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isForbidden());
  }

  @Test
  void getDebtPositionByIUPD_404() throws Exception {
    String url = "/organizations/200_12345678901/debtpositions/12345678901IUPDNOTEXIST";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  /** GET LIST DEBT POSITIONS */
  @Test
  void getDebtPositionList() throws Exception {
    // creo due posizioni debitorie e recupero tutte le payment_option di entrambe
    mvc.perform(
            post("/organizations/LIST_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock2()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post("/organizations/LIST_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/LIST_12345678901/debtpositions?page=0"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS))
            + "&orderby=IUPD&ordering=ASC";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd")
                .value(Matchers.hasSize(2)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
                .value(Matchers.hasSize(2)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
                .value(Matchers.hasSize(3)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[0].nav")
                .value("31234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[1].nav")
                .value("31234562"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[0].nav")
                .value("31234563"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[1].nav")
                .value("31234564"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[2].nav")
                .value("31234565"));
  }

  @Test
  void getDebtPositionList_NAV() throws Exception {

    // creo due posizioni debitorie di cui una con il NAV settato in fase di creazione e l'altra con
    // il default <AUX_DIGIT>+IUV
    PaymentPositionDTO ppNav = DebtPositionMock.getMock2();
    ppNav.getPaymentOption().forEach(po -> po.setNav("555" + auxDigit + po.getIuv()));
    mvc.perform(
            post("/organizations/LIST_NAV_12345678901/debtpositions")
                .content(TestUtil.toJson(ppNav))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post("/organizations/LIST_NAV_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/LIST_NAV_12345678901/debtpositions?page=0"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS))
            + "&orderby=IUPD&ordering=ASC";

    // recupero le payment_option di entrambe e verifico che una abbia il NAV custom e l'altra in
    // NAV di default
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd")
                .value(Matchers.hasSize(2)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
                .value(Matchers.hasSize(2)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
                .value(Matchers.hasSize(3)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[0].nav")
                .value("555" + auxDigit + "1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[1].nav")
                .value("555" + auxDigit + "1234562"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[0].nav")
                .value(auxDigit + "1234563"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[1].nav")
                .value(auxDigit + "1234564"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[2].nav")
                .value(auxDigit + "1234565"));
  }

  @Test
  void getDebtPositionList_UpdateDateInterval() throws Exception {
    // creo due posizioni debitorie e recupero tutte le payment_option di entrambe, inserendo nel
    // filtro solo la due_date_from
    mvc.perform(
            post("/organizations/LIST_12345678904/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock2()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post("/organizations/LIST_12345678904/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/LIST_12345678904/debtpositions?page=0"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&orderby=IUPD&ordering=ASC";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd")
                .value(Matchers.hasSize(2)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
                .value(Matchers.hasSize(2)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
                .value(Matchers.hasSize(3)));
  }

  @Test
  void getDebtPositionListByPaymentDate() throws Exception {
    // creo la posizione debitoria DRAFT
    mvc.perform(
            post("/organizations/123456789022/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // creo la posizione debitoria (senza 'validity date' impostata) che dopo il pagamento sarà PAID
    mvc.perform(
            post("/organizations/123456789022/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/123456789022/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento
    mvc.perform(
            post("/organizations/123456789022/paymentoptions/"
                    + auxDigit
                    + "1234561/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));

    // effettuo la chiamata GET applicando il filtro sulla payment_date
    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/123456789022/debtpositions?page=0"
            + "&payment_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&payment_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plusDays(9));
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list").value(Matchers.hasSize(1)));
  }

  @Test
  void getDebtPositionListByPaymentDateTime() throws Exception {
    // creo la posizione debitoria DRAFT
    mvc.perform(
            post("/organizations/DATE_TIME_123456789022/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // creo la posizione debitoria (senza 'validity date' impostata) che dopo il pagamento sarà PAID
    mvc.perform(
            post("/organizations/DATE_TIME_123456789022/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/DATE_TIME_123456789022/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento
    mvc.perform(
            post("/organizations/DATE_TIME_123456789022/paymentoptions/"
                + auxDigit
                + "1234561/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));

    // effettuo la chiamata GET applicando il filtro sulla payment_date_time
    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");
    String url =
        "/organizations/DATE_TIME_123456789022/debtpositions?page=0"
            + "&payment_date_time_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&payment_date_time_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plusDays(9));
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list").value(Matchers.hasSize(1)));
  }

  @Test
  void getDebtPositionListByStatus() throws Exception {
    // creo la posizione debitoria DRAFT
    mvc.perform(
            post("/organizations/123456789030/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // creo la posizione debitoria (senza 'validity date' impostata) che sarà PAID dopo il pagamento
    mvc.perform(
            post("/organizations/123456789030/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/123456789030/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento
    mvc.perform(
            post("/organizations/123456789030/paymentoptions/"
                    + auxDigit
                    + "1234561/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la chiamata GET applicando il filtro sullo status
    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/123456789030/debtpositions?page=0"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS))
            + "&status=PAID";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list").value(Matchers.hasSize(1)));
  }

  @Test
  void getDebtPositionListOrdered() throws Exception {
    // creo due posizioni debitorie e le recupero con ordinamento
    mvc.perform(
            post("/organizations/LIST_ORDERED_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock2()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post("/organizations/LIST_ORDERED_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/LIST_ORDERED_12345678901/debtpositions?page=0&orderby=INSERTED_DATE&ordering=DESC"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd")
                .value(Matchers.hasSize(2)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].iupd")
                .value("12345678901IUPDMULTIPLEMOCK2"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].iupd")
                .value("12345678901IUPDMULTIPLEMOCK1"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
                .value(Matchers.hasSize(3)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
                .value(Matchers.hasSize(2)));
  }

  @Test
  void getDebtPositionList_partial() throws Exception {
    // creo due posizioni debitorie ed estraggo per intervallo di date che non comprende tutte le
    // payment_option create
    mvc.perform(
            post("/organizations/DUEDATEBETWEEN_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock2()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post("/organizations/DUEDATEBETWEEN_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/DUEDATEBETWEEN_12345678901/debtpositions?page=0"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.DAYS))
            + "&orderby=IUPD&ordering=ASC";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd")
                .value(Matchers.hasSize(2)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
                // manca la payment_option che ha una due_date maggiore di quella inserita nella
                // ricerca
                .value(Matchers.hasSize(1)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[1].paymentOption[*].iuv")
                .value(Matchers.hasSize(3)));
  }

  @Test
  void getDebtPositionList_404() throws Exception {
    // provo a recuperare una posizione debitoria con una url sbagliata
    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/LIST404_12345678901/debtpositions"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isNotFound());
  }

  @Test
  void getDebtPositionListDueDate_400() throws Exception {
    // provo a recuperare una posizione debitoria passando le date con un formato diverso da quello
    // atteso
    DateTimeFormatter df = DateTimeFormatter.ofPattern("dd-MM-yyyy");
    String url =
        "/organizations/LIST404_12345678901/debtpositions?page=0"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS));
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void getDebtPositionListDueDate_Interval_400() throws Exception {
    // provo a recuperare una posizione debitoria passando un intervallo di date troppo ampio
    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/LIST404_12345678901/debtpositions?page=0"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(60, ChronoUnit.DAYS));
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void getDebtPositionListDueDate_InversionDate_400() throws Exception {
    // provo a recuperare una posizione debitoria passando un intervallo di date invertito (from >
    // to)
    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/LIST404_12345678901/debtpositions?page=0"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(60, ChronoUnit.DAYS))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC));
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void getDebtPositionList_MutualExclusion_400() throws Exception {
    // provo a recuperare una posizione debitoria passando sia l'intervallo di date due_date che
    // payment_date
    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String url =
        "/organizations/LIST404_12345678901/debtpositions?page=0"
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC))
            + "&payment_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plus(9, ChronoUnit.DAYS))
            + "&payment_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC));
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void getDebtPositionListBySegregationCode_200() throws Exception {
    PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock2();
    String iuv1 = paymentPositionDTO.getPaymentOption().get(0).getIuv();
    String iuv2 = paymentPositionDTO.getPaymentOption().get(1).getIuv();
    String firstSegregationCode =
        paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0, 2);
    // Create 2 DEBT POSITION but GET only the one related to the given segregation code because the
    // caller is authorized only for the first
    mvc.perform(
            post("/organizations/LIST_SC_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String url =
        "/organizations/LIST_SC_12345678901/debtpositions?page=0&segregationCodes="
            + firstSegregationCode;
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd")
                .value(Matchers.hasSize(1)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[*].iuv")
                .value(Matchers.hasSize(2)))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[0].nav")
                .value("3" + iuv1))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[0].paymentOption[1].nav")
                .value("3" + iuv2));
  }

  @Test
  void getDebtPositionListBySegregationCode_403() throws Exception {
    PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock2();
    String firstSegregationCode = "99";
    // Create 2 DEBT POSITION but GET only the one related to the given segregation code because the
    // caller is authorized only for the first
    mvc.perform(
            post("/organizations/LIST_403_SC_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post("/organizations/LIST_403_SC_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String url =
        "/organizations/LIST_403_SC_12345678901/debtpositions?page=0&segregationCodes="
            + firstSegregationCode;
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.payment_position_list[*].iupd")
                .value(Matchers.hasSize(0)));
  }

  /** DELETE DEBT POSITION */
  @Test
  void deleteDebtPosition_200() throws Exception {
    // creo una posizione debitoria e la cancello
    mvc.perform(
            post("/organizations/DEL_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            delete("/organizations/DEL_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

  @Test
  void deleteDebtPosition_404() throws Exception {
    // provo a cancellare una posizione debitoria che non esiste
    mvc.perform(
            delete("/organizations/DEL_12345678901/debtpositions/12345678901IUPDNOTEXIST")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound());
  }

  @Test
  void deleteDebtPosition_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata)
    mvc.perform(
            post("/organizations/DEL_409_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/DEL_409_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/DEL_409_12345678901/paymentoptions/"
                    + auxDigit
                    + "1234561/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/DEL_409_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // provo a cancellare la posizione debitoria che ha già un pagamento in essere
    mvc.perform(
            delete("/organizations/DEL_409_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void deleteDebtPosition_SegregationCode_200() throws Exception {
    PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
    String segregationCode = paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0, 2);
    mvc.perform(
            post("/organizations/DEL_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            delete(
                    "/organizations/DEL_12345678901/debtpositions/12345678901IUPDMOCK1?segregationCodes="
                        + segregationCode)
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

  @Test
  void deleteDebtPosition_SegregationCode_403() throws Exception {
    PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
    String segregationCode = "99";
    mvc.perform(
            post("/organizations/DEL_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            delete(
                    "/organizations/DEL_12345678901/debtpositions/12345678901IUPDMOCK1?segregationCodes="
                        + segregationCode)
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isForbidden());
  }

  /** UPDATE DEBT POSITION */
  @Test
  void updateDebtPosition_200() throws Exception {
    // creo una posizione debitoria con validatyDate a null e senza forzare il toPublish
    mvc.perform(
            post("/organizations/UPD_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.nullValue()));

    // recupero la posizione debitoria e verifico il contenuto
    mvc.perform(
            get("/organizations/UPD_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.companyName").value("Comune di Firenze"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv").value(Matchers.hasSize(1)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount").value(1000))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount").value(1000))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.nullValue()));

    // aggiorno la posizione debitoria
    mvc.perform(
            put("/organizations/UPD_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(DebtPositionMock.getMock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che il nuovo contenuto
    mvc.perform(
            get("/organizations/UPD_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.companyName").value("Comune di Roma"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv").value(Matchers.hasSize(2)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount").value(1000))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount").value(1000))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].amount").value(500))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[1].transfer[0].amount").value(500))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[1].nav")
                .value(auxDigit + "1234562"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.nullValue()));
  }

  @Test
  void updateDebtPositionWithMetadata_200() throws Exception {
    // creo una posizione debitoria con metadati su PO e transfer
    mvc.perform(
            post("/organizations/200_UPD_metadata_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMetadataMock8()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234569"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));

    // recupero la posizione debitoria e verifico siano presenti i metadati inseriti
    String url =
        "/organizations/200_UPD_metadata_12345678901/debtpositions/12345678901IUPDMETADATAMOCK7";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234569"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));

    // aggiorno la posizione debitoria aggiungendo dei metadati
    PaymentPositionDTO ppToUpdate = DebtPositionMock.getMetadataMock8();
    ppToUpdate
        .getPaymentOption()
        .get(0)
        .addPaymentOptionMetadata(
            PaymentOptionMetadataDTO.builder().key("keypometadataupd").value("valuepometadataupd").build());
    ppToUpdate
        .getPaymentOption()
        .get(0)
        .getTransfer()
        .get(0)
        .addTransferMetadata(TransferMetadataDTO.builder().key("keytransfermetadataupd").value("valuetransfermetadataupd").build());
    mvc.perform(
            put("/organizations/200_UPD_metadata_12345678901/debtpositions/12345678901IUPDMETADATAMOCK7")
                .content(TestUtil.toJson(ppToUpdate))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234569"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[1].key")
                .value("keypometadataupd"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[1].key")
                .value("keytransfermetadataupd"));

    // recupero la posizione debitoria e verifico siano presenti i metadati inseriti
    url = "/organizations/200_UPD_metadata_12345678901/debtpositions/12345678901IUPDMETADATAMOCK7";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234569"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[1].key")
                .value("keypometadataupd"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[1].key")
                .value("keytransfermetadataupd"));
  }

  @Test
  void updateDebtPosition_SegregationCode_200() throws Exception {
    PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
    String iupd = paymentPositionDTO.getIupd();
    String iuv = paymentPositionDTO.getPaymentOption().get(0).getIuv();
    String segregationCode = paymentPositionDTO.getPaymentOption().get(0).getIuv().substring(0, 2);
    mvc.perform(
            post("/organizations/UPD_SC_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value('3' + iuv));

    // aggiorno la posizione debitoria
    mvc.perform(
            put("/organizations/UPD_SC_12345678901/debtpositions/"
                    + iupd
                    + "?segregationCodes="
                    + segregationCode)
                .content(TestUtil.toJson(DebtPositionMock.getMock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che il nuovo contenuto
    mvc.perform(
            get("/organizations/UPD_SC_12345678901/debtpositions/" + iupd)
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.companyName").value("Comune di Roma"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv").value(Matchers.hasSize(2)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount").value(1000))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount").value(1000))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[1].amount").value(500))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[1].transfer[0].amount").value(500))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("31234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[1].nav")
                .value("31234562"));
  }

  @Test
  void updateDebtPosition_NAV_200() throws Exception {
    // creo una posizione debitoria con un default NAV <AUX_DIGIT>+IUV
    mvc.perform(
            post("/organizations/UPD_NAV_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"));

    // recupero la posizione debitoria e verifico il contenuto
    mvc.perform(
            get("/organizations/UPD_NAV_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.companyName").value("Comune di Firenze"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv").value(Matchers.hasSize(1)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount").value(1000))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount").value(1000))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"));

    // aggiorno la posizione debitoria con un custom NAV
    PaymentPositionDTO ppNav = DebtPositionMock.getMock1();
    ppNav.getPaymentOption().forEach(po -> po.setNav("CUSTOM_" + auxDigit + po.getIuv()));
    mvc.perform(
            put("/organizations/UPD_NAV_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(ppNav))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che il nuovo contenuto
    mvc.perform(
            get("/organizations/UPD_NAV_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.companyName").value("Comune di Firenze"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv").value(Matchers.hasSize(1)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount").value(1000))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].amount").value(1000))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value("CUSTOM_" + auxDigit + "1234561"));
  }

  @Test
  void updateDebtPosition_change_officeName_200() throws Exception {
    PaymentPositionDTO pp = DebtPositionMock.getMock1();

    // creo una posizione debitoria
    mvc.perform(
            post("/organizations/UPD_office_12345678901/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // recupero la posizione debitoria e verifico il contenuto
    mvc.perform(
            get("/organizations/UPD_office_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.companyName").value("Comune di Firenze"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.officeName").value("Ufficio tributario"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv").value(Matchers.hasSize(1)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount").value(1000));

    // aggiorno il nome dell'ufficio per la posizione debitoria
    pp.setOfficeName("Agenzia delle entrate");
    mvc.perform(
            put("/organizations/UPD_office_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che l'aggiornamento dell'ufficio sia avvenuto correttamente
    mvc.perform(
            get("/organizations/UPD_office_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.companyName").value("Comune di Firenze"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.officeName").value("Agenzia delle entrate"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[*].iuv").value(Matchers.hasSize(1)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].amount").value(1000));
  }

  @Test
  void updateDebtPosition_Published_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata)
    mvc.perform(
            post("/organizations/UPD_PBH_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // recupero la posizione debitoria e verifico lo stato in draft
    mvc.perform(
            get("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che lo stato sia stato aggiornato a valid (doppio passaggio di stato)
    mvc.perform(
            get("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

    // aggiorno la posizione debitoria con toPublish = false
    mvc.perform(
            put("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(DebtPositionMock.getMock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // recupero la posizione debitoria e verifico che lo stato sia tornato in draft
    mvc.perform(
            get("/organizations/UPD_PBH_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());
  }

  @Test
  void updateDebtPosition_CreateAndPublished_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata)
    mvc.perform(
            post("/organizations/MRDPLL54H17D542L/debtpositions?toPublish=True")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // recupero la posizione debitoria e verifico lo stato in valid
    mvc.perform(
            get("/organizations/MRDPLL54H17D542L/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.notNullValue()));

    // aggiorno la posizione debitoria con un body che non contiene la 'validity date'
    mvc.perform(
            put("/organizations/MRDPLL54H17D542L/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(DebtPositionMock.getMock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // recupero la posizione debitoria e verifico che lo stato sia rimasto in DRAFT
    mvc.perform(
            get("/organizations/MRDPLL54H17D542L/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());
  }

  @Test
  void updateDebtPosition_CreateValidAndUpdate_200() throws Exception {
    // creo una posizione debitoria (con 'validity date' impostata)
    PaymentPositionDTO ppDto1 = DebtPositionMock.getMock1();
    
    LocalDateTime futureValidity = LocalDateTime.now(ZoneOffset.UTC).plus(1, ChronoUnit.DAYS).truncatedTo(ChronoUnit.SECONDS);
    LocalDateTime futureDue = LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.DAYS);

    ppDto1.setValidityDate(futureValidity);
    ppDto1.getPaymentOption().forEach(opt -> {
    	opt.setStatus(PaymentOptionStatus.PO_UNPAID);
    	opt.setSwitchToExpired(true);
    	opt.setDueDate(futureDue);
    	opt.setValidityDate(futureValidity);
    });
    mvc.perform(
            post("/organizations/CREATE_UPD_12345678901/debtpositions?toPublish=True")
                .content(TestUtil.toJson(ppDto1))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // recupero la posizione debitoria e verifico lo stato in published
    mvc.perform(
            get("/organizations/CREATE_UPD_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.validityDate")
                .value(futureValidity.format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"))));

    // aggiorno la posizione debitoria con un body che contiene la 'validity date'
    PaymentPositionDTO ppDto4 = DebtPositionMock.getMock4();
    ppDto4.setValidityDate(futureValidity);
    ppDto4.getPaymentOption().forEach(opt -> {
    	opt.setStatus(PaymentOptionStatus.PO_UNPAID);
    	opt.setSwitchToExpired(true);
    	opt.setDueDate(futureDue);
    	opt.setValidityDate(futureValidity);
    });
    mvc.perform(
            put("/organizations/CREATE_UPD_12345678901/debtpositions/12345678901IUPDMOCK1?toPublish=True")
                .content(TestUtil.toJson(ppDto4))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()));

    // recupero la posizione debitoria e verifico che lo stato sia in PUBLISHED
    mvc.perform(
            get("/organizations/CREATE_UPD_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.validityDate")
                .value(futureValidity.format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"))))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());
  }

  @Test
  void updateAndPublishDebtPosition_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata)
    mvc.perform(
            post("/organizations/UPDANDPBH_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // recupero la posizione debitoria e verifico lo stato in draft
    mvc.perform(
            get("/organizations/UPDANDPBH_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty())
        .andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.nullValue()));

    // aggiorno e porto direttamente in pubblicata la posizione debitoria
    mvc.perform(
            put("/organizations/UPDANDPBH_12345678901/debtpositions/12345678901IUPDMOCK1?toPublish=true")
                .content(TestUtil.toJson(DebtPositionMock.getMock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));

    // verifico che lo stato sia stato settato a valid
    mvc.perform(
            get("/organizations/UPDANDPBH_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty())
        .andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.notNullValue()));

    // provo a fare una nuova pubblicazione su una posizione debitoria con uno stato non più idoneo
    mvc.perform(
            post("/organizations/UPDANDPBH_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void updateDebtPosition_SegregationCode_403() throws Exception {
    PaymentPositionDTO paymentPositionDTO = DebtPositionMock.getMock1();
    String iupd = paymentPositionDTO.getIupd();
    String iuv = paymentPositionDTO.getPaymentOption().get(0).getIuv();
    String notSufficientSegregationCode = "99";
    mvc.perform(
            post("/organizations/UPD_403_SC_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav").value(auxDigit + iuv));

    mvc.perform(
            put("/organizations/UPD_403_SC_12345678901/debtpositions/"
                    + iupd
                    + "?segregationCodes="
                    + notSufficientSegregationCode)
                .content(TestUtil.toJson(DebtPositionMock.getMock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isForbidden());
  }

  @Test
  void updateDebtPosition_400() throws Exception {
    // chiamata di aggiornamento della posizione debitoria con IUPD differenti
    mvc.perform(
            put("/organizations/400_12345678901/debtpositions/FAKE_12345678901IUPDMOCK1")
                .content(TestUtil.toJson(DebtPositionMock.getMock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());
  }

  @Test
  void updateDebtPosition_No_Body_400() throws Exception {
    // chiamata di aggiornamento della posizione debitoria con IUPD differenti
    mvc.perform(
            put("/organizations/400_12345678901/debtpositions/FAKE_12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());
  }

  @Test
  void updateDebtPosition_Invalid_Input_400() throws Exception {
    // creo una posizione debitoria
    mvc.perform(
            post("/organizations/400_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // aggiorno la posizione debitoria mettendo una due_date < current date -> devo ricevere errore
    // 400
    mvc.perform(
            put("/organizations/400_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());
  }

  @Test
  void updateDebtPosition_404() throws Exception {
    // chiamata di aggiornamento della posizione debitoria con IUPD inesistente
    mvc.perform(
            put("/organizations/UPD_12345678901/debtpositions/12345678901IUPD400MOCK1")
                .content(TestUtil.toJson(DebtPositionMock.get400Mock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound());
  }

  @Test
  void updateDebtPosition_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata)
    mvc.perform(
            post("/organizations/UPD409_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // recupero la posizione debitoria e verifico lo stato in draft
    mvc.perform(
            get("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isEmpty());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che lo stato sia stato aggiornato a valid (doppio passaggio di stato)
    mvc.perform(
            get("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

    // invalido la posizione debitoria
    mvc.perform(
            post("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1/invalidate")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.INVALID.toString()));

    // aggiorno la posizione debitoria con stato INVALID -> errore 409 (non deve essere possibile)
    mvc.perform(
            put("/organizations/UPD409_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(DebtPositionMock.getMock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void updateDebtPosition_PAID_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata)
    mvc.perform(
            post("/organizations/UPD409_PAID_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/UPD409_PAID_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/UPD409_PAID_12345678901/paymentoptions/"
                    + auxDigit
                    + "1234561/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/UPD409_PAID_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // provo ad aggiornare la posizione debitoria con stato già in PAID -> errore 409 (non deve
    // essere possibile)
    mvc.perform(
            put("/organizations/UPD409_PAID_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(DebtPositionMock.getMock4()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void updateDebtPosition_noValidTransfer_422() throws Exception {

    PaymentPositionDTO paymentPositionDTO =
        DebtPositionMock.paymentPositionForNotificationUpdateMock1();

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenReturn(NodeCheckPositionResponse.builder().outcome("OK").build());

    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/UPD422_novalidtransfer_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // effettuo un aggiornamento della notification fee (si continua ad utilizzare lo IUV e non il
    // NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/UPD422_novalidtransfer_12345678901/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(150L)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la chiamata di modifica del codice fiscale dell'EC del transfer ma non posso
    // procedere perche eliminerei tutti i transfer associabili per la fee
    paymentPositionDTO
        .getPaymentOption()
        .get(0)
        .getTransfer()
        .get(0)
        .setOrganizationFiscalCode("acreditorinstitution");
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/UPD422_novalidtransfer_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isUnprocessableEntity());
  }

  /** UPDATE IBAN ON TRANSFERS */
  @Test
  void updateTransferIbanMassive_200() throws Exception {
    UpdateTransferIbanMassiveModel request =
        UpdateTransferIbanMassiveModel.builder().newIban("XYZ").build();

    doReturn(1)
        .when(paymentPositionService)
        .updateTransferIbanMassive("77777777777", "ABCDE", "XYZ", 10);

    mvc.perform(
            patch("/organizations/77777777777/debtpositions/transfers?oldIban=ABCDE&limit=10")
                .content(TestUtil.toJson(request))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.updatedTransfers").value(1));
  }

  @Test
  void updateTransferIbanMassive_400_noOldIban() throws Exception {
    UpdateTransferIbanMassiveModel request =
        UpdateTransferIbanMassiveModel.builder().newIban("XYZ").build();

    mvc.perform(
            patch("/organizations/notFoundOrg/debtpositions/transfers")
                .content(TestUtil.toJson(request))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());
  }

  @Test
  void updateTransferIbanMassive_400_overMaxLimit() throws Exception {
    UpdateTransferIbanMassiveModel request =
        UpdateTransferIbanMassiveModel.builder().newIban("XYZ").build();

    mvc.perform(
            patch("/organizations/notFoundOrg/debtpositions/transfers?oldIban=ABCDE&limit=10000")
                .content(TestUtil.toJson(request))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());
  }

  @Test
  void updateTransferIbanMassive_400_noNewIban() throws Exception {
    UpdateTransferIbanMassiveModel request =
        UpdateTransferIbanMassiveModel.builder().newIban(null).build();

    mvc.perform(
            patch("/organizations/notFoundOrg/debtpositions/transfers?oldIban=ABCDE")
                .content(TestUtil.toJson(request))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());
  }
}
