package it.gov.pagopa.debtposition.controller;

import static org.assertj.core.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import feign.FeignException;
import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.client.NodeClient;
import it.gov.pagopa.debtposition.client.SendClient;
import it.gov.pagopa.debtposition.dto.PaymentOptionDTO;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.dto.TransferDTO;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.mock.DebtPositionMock;
import it.gov.pagopa.debtposition.model.checkposition.NodeCheckPositionModel;
import it.gov.pagopa.debtposition.model.checkposition.response.NodeCheckPositionResponse;
import it.gov.pagopa.debtposition.model.config.Notice;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.pd.NotificationFeeUpdateModel;
import it.gov.pagopa.debtposition.model.send.response.NotificationPriceResponse;
import it.gov.pagopa.debtposition.service.payments.PaymentsService;
import it.gov.pagopa.debtposition.util.CustomHttpStatus;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class PaymentsControllerTest {

  @Autowired private MockMvc mvc;

  @Mock private ModelMapper modelMapperMock;

  @MockBean private NodeClient nodeClient;

  @MockBean private SendClient sendClient;

  @SpyBean private PaymentsService paymentsService;

  @Value("${nav.aux.digit}")
  private String auxDigit;

  @BeforeEach
  void setUp() {}

  /** GET PAYMENT OPTION BY NAV */
  @Test
  void getPaymentOptionByNAV_200() throws Exception {
    // creo una posizione debitoria con NAV custom e recupero la payment option associata
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    pp.getPaymentOption().forEach(po -> po.setNav("CUSTOMNAV_" + po.getIuv()));
    mvc.perform(
            post("/organizations/PO200_12345678901/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(result -> {
          int status = result.getResponse().getStatus();
          if (status != HttpStatus.CREATED.value() && status != HttpStatus.CONFLICT.value()) {
            throw new AssertionError("Expected status 201 (Created) or 409 (Conflict), but got: " + status);
          }
        });

    NotificationPriceResponse priceRes = new NotificationPriceResponse("IUN", 1, 1, 0, 0, LocalDateTime.now(),  LocalDateTime.now(),  1, 1);
    when(sendClient.getNotificationFee(anyString(), anyString()))
            .thenReturn(priceRes);

    String url = "/organizations/PO200_12345678901/paymentoptions/CUSTOMNAV_123456IUVMOCK1";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value("CUSTOMNAV_123456IUVMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"));
  }

  @Test
  void getPaymentOptionByNAVSendSyncTimeoutException_200() throws Exception {
    // creo una posizione debitoria con NAV custom e recupero la payment option associata
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    String organization = "PO200_12345678901";

    for (PaymentOptionDTO po : pp.getPaymentOption()) {
      po.setNav("CUSTOMNAV_" + po.getIuv());
    }

    mvc.perform(post("/organizations/" + organization + "/debtpositions")
                    .content(TestUtil.toJson(pp))
                    .contentType(MediaType.APPLICATION_JSON));

    for (PaymentOptionDTO po : pp.getPaymentOption()) {
      ArrayList<Notice> notices = new ArrayList<>();
      notices.add(new Notice(organization, po.getNav()));
      mvc.perform(post("/internal/config/send")
                      .contentType(MediaType.APPLICATION_JSON)
                      .content(new ObjectMapper().writeValueAsString(notices)))
              .andExpect(status().isOk());
    }

    when(sendClient.getNotificationFee(anyString(), anyString()))
            .thenThrow(new RuntimeException("Connection timeout"));

    String url = "/organizations/" + organization + "/paymentoptions/CUSTOMNAV_123456IUVMOCK1";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value("CUSTOMNAV_123456IUVMOCK1"))
            .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"));
  }

  @Test
  void getPaymentOptionByNAVSendSyncOk_200() throws Exception {
    // creo una posizione debitoria con NAV custom e recupero la payment option associata
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    String organization = "PO200_12345678901";

    for (PaymentOptionDTO po : pp.getPaymentOption()) {
      po.setNav("CUSTOMNAV_" + po.getIuv());
    }

    mvc.perform(post("/organizations/" + organization + "/debtpositions")
            .content(TestUtil.toJson(pp))
            .contentType(MediaType.APPLICATION_JSON));

    for (PaymentOptionDTO po : pp.getPaymentOption()) {
      ArrayList<Notice> notices = new ArrayList<>();
      notices.add(new Notice(organization, po.getNav()));
      mvc.perform(post("/internal/config/send")
                      .contentType(MediaType.APPLICATION_JSON)
                      .content(new ObjectMapper().writeValueAsString(notices)))
              .andExpect(status().isOk());
    }

    long firstPOAmount = pp.getPaymentOption().get(0).getAmount();
    String firstPONav = pp.getPaymentOption().get(0).getNav();
    NotificationPriceResponse priceRes = new NotificationPriceResponse("IUN", 1, 1, 0, 0, LocalDateTime.now(),  LocalDateTime.now(),  1, 1);
    Integer price = priceRes.getTotalPrice();
    when(sendClient.getNotificationFee(anyString(), anyString()))
            .thenReturn(priceRes);

    // Get first Payment Option
    String url = "/organizations/" + organization + "/paymentoptions/" + firstPONav;
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andExpect(content().contentType(MediaType.APPLICATION_JSON))
            .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value("CUSTOMNAV_123456IUVMOCK1"))
            .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
            .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value(firstPOAmount + price));
  }

  @Test
  void getPaymentOptionByNAV_MultiplePO_200() throws Exception {
    // creo una posizione debitoria con NAV custom e con più payment option associate
    PaymentPositionDTO pp = DebtPositionMock.getMock2();
    pp.getPaymentOption().forEach(po -> po.setNav("CUSTOMNAV_" + po.getIuv()));
    mvc.perform(
            post("/organizations/PO200_Multiple_12345678901/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // ne recupero una e verifico sia quella attesa
    String url =
        "/organizations/PO200_Multiple_12345678901/paymentoptions/CUSTOMNAV_123456IUVMULTIPLEMOCK2";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value("CUSTOMNAV_123456IUVMULTIPLEMOCK2"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK2"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iupd").value("12345678901IUPDMULTIPLEMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value("500"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_UNPAID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.transfer[*]").value(Matchers.hasSize(1)));
  }

  @Test
  void getPaymentOptionByNAV_POPAID_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/POPAID_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/POPAID_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid e il nav valorizzato nel
    // formato <AUX_DIGIT>+IUV
    mvc.perform(
            post("/organizations/POPAID_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero la payment option e verifico di nuovo lo stato in paid e il nav valorizzato nel
    // formato <AUX_DIGIT>+IUV
    String url = "/organizations/POPAID_12345678901/paymentoptions/" + auxDigit + "123456IUVMOCK1";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iupd").value("12345678901IUPDMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value("1000"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.transfer[*]").value(Matchers.hasSize(1)));
  }

  @Test
  void getPaymentOptionByNAV_404() throws Exception {
    String url = "/organizations/PO200_12345678901/paymentoptions/CUSTOMNAV_123456_NOTEXIST";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void getPaymentOptionWithMetadataByNAV_POPAID_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/POPAID_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMetadataMock8()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "123456IUVMETADATAMOCK9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].iuv")
                .value("123456IUVMETADATAMOCK9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/POPAID_12345678901/debtpositions/12345678901IUPDMETADATAMOCK7/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/POPAID_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMETADATAMOCK9/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMETADATAMOCK9"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMETADATAMOCK9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));

    // recupero la payment option e verifico di nuovo lo stato in paid
    String url =
        "/organizations/POPAID_12345678901/paymentoptions/" + auxDigit + "123456IUVMETADATAMOCK9";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMETADATAMOCK9"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMETADATAMOCK9"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iupd").value("12345678901IUPDMETADATAMOCK7"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value("1000"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.transfer[*]").value(Matchers.hasSize(1)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOptionMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOptionMetadata[0].key")
                .value("keypometadatamock9"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].transferMetadata[0].key")
                .value("keytransfermetadatamock3"));

    // effettuo la rendicontazione per l'unica transazione della PO con id 3 (la chiamata report
    // lavora con lo IUV e non con il NAV)
    mvc.perform(
            post("/organizations/POPAID_12345678901/paymentoptions/123456IUVMETADATAMOCK9/transfers/3/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.transferMetadata").isArray())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transferMetadata[0].key")
                .value("keytransfermetadatamock3"));
  }

  /** PAY A PAYMENT OPTION */
  @Test
  void payPaymentOption_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/PAY_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "123456IUVMOCK1"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].iuv").value("123456IUVMOCK1"));

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PAY_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/PAY_12345678901/paymentoptions/" + auxDigit + "123456IUVMOCK1/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/PAY_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));
  }

  @Test
  void payPaymentOption_200_with_only_required_receipt_fields() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/PAY_12345678911/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PAY_12345678911/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    PaymentOptionDTO data = DebtPositionMock.getPayPOMock1();
    data.setPaymentDate(null);
    data.setPaymentMethod(null);
    data.setFee(0); // because it is a long initialized to 0
    mvc.perform(
            post("/organizations/PAY_12345678911/paymentoptions/" + auxDigit + "123456IUVMOCK1/pay")
                .content(TestUtil.toJson(data))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/PAY_12345678911/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));
  }

  @Test
  void payPaymentOption_Multiple_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/PAY_Multiple_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PAY_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/PAY_Multiple_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK3/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK3"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/PAY_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "123456IUVMULTIPLEMOCK3"));
  }

  @Test
  void payPaymentOption_Multiple_Partial_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK4/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK4"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/PAY_Multiple_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));
  }

  @Test
  void payPaymentOption_Multiple_All_Partial_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/PAY_Multiple_All_Partial_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PAY_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/PAY_Multiple_All_Partial_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK4/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK4"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/PAY_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo la notifica di pagamento della seconda rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/PAY_Multiple_All_Partial_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK5/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK5"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK5"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
    mvc.perform(
            get("/organizations/PAY_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));
  }

  @Test
  void payPaymentOption_Multiple_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/PAY_Multiple_409_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PAY_Multiple_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/PAY_Multiple_409_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK3/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK3"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/PAY_Multiple_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo un nuovo pagamento per la stessa payment option
    mvc.perform(
            post("/organizations/PAY_Multiple_409_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK3/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void payPaymentOption_Multiple_Partial_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial_409_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial_409_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK3/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK3"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/PAY_Multiple_Partial_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo un nuovo pagamento su una delle rate parziali (setIsPartialPayment = true) per la
    // payment option
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial_409_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK4/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void getPaymentOption_Multiple_Partial_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial2_409_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial2_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (isPartialPayment = true) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial2_409_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK4/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK4"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/PAY_Multiple_Partial2_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo una GET/ACTIVATE sulla payment option corrispondente alla rata unica/intero importo
    // e ottengo errore 409
    mvc.perform(
            get("/organizations/PAY_Multiple_Partial2_409_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK3")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void payPaymentOption_ok() throws Exception {
    // creo una posizione debitoria (con 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/PAY_422_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock6()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PAY_422_12345678901/debtpositions/12345678901IUPDMOCK5/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // l'errore 422 di 'Not in payable state'
    mvc.perform(
            post("/organizations/PAY_422_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK6/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

  @Test
  void payPaymentOption_404() throws Exception {
    // provo a pagare una payment option che non esiste
    String url = "/organizations/PAY_400_12345678901/paymentoptions/123456_NAV_NOTEXIST";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void payPaymentOption_400() throws Exception {
    // provo a pagare una payment option con body della request non corretto
    mvc.perform(
            post("/organizations/PAY_Multiple_Partial2_409_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK4/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPO400Mock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  /** REPORT A TRANSFER */
  @Test
  void reportTransfer_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con una
    // sola PO e isPartialPayment=false
    mvc.perform(
            post("/organizations/REPORT_200_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/REPORT_200_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/REPORT_200_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/REPORT_200_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per l'unica transazione della PO (si continua ad utilizzare lo
    // IUV e non il NAV)
    mvc.perform(
            post("/organizations/REPORT_200_12345678901/paymentoptions/123456IUVMOCK1/transfers/1/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_REPORTED
    String url =
        "/organizations/REPORT_200_12345678901/paymentoptions/" + auxDigit + "123456IUVMOCK1";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in reported
    mvc.perform(
            get("/organizations/REPORT_200_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.REPORTED.toString()));
  }

  @Test
  void reportTransfer_Multiple_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/REPORT_Multiple_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/REPORT_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/REPORT_Multiple_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK3/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK3"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/REPORT_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per l'unica transazione della PO (si continua ad utilizzare lo
    // IUV e non il NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/transfers/3/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_REPORTED
    String url =
        "/organizations/REPORT_Multiple_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK3";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in reported
    mvc.perform(
            get("/organizations/REPORT_Multiple_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.REPORTED.toString()));
  }

  @Test
  void reportTransfer_Multiple_Partial_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/REPORT_Multiple_Partial_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/REPORT_Multiple_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/REPORT_Multiple_Partial_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK4/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK4"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/REPORT_Multiple_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo la rendicontazione per la transazione (si continua ad utilizzare lo IUV e non il
    // NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
    String url =
        "/organizations/REPORT_Multiple_Partial_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK4";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PARTIALLY_REPORTED.toString()));
  }

  @Test
  void reportTransfer_Multiple_All_Partial_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK4/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK4"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo la notifica di pagamento della seconda rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK5/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK5"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK5"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
    mvc.perform(
            get("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per una delle 2 transazioni della PO (si continua ad utilizzare
    // lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
    String url =
        "/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK4";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PARTIALLY_REPORTED.toString()));

    // effettuo la rendicontazione per la seconda delle 2 transazioni della PO (si continua ad
    // utilizzare lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/5/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato sia passato in PO_REPORTED
    url =
        "/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK4";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero la PO non ancora rendicontata della posizione debitoria e verifico che sia ancora in
    // PAID
    url =
        "/organizations/REPORT_Multiple_All_Partial_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK5";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia ancora in paid
    mvc.perform(
            get("/organizations/REPORT_Multiple_All_Partial_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));
  }

  @Test
  void reportTransfer_Multiple_All_Partial_Reported_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK4/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK4"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK4"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo la notifica di pagamento della seconda rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK5/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK5"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK5"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
    mvc.perform(
            get("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per una delle 2 transazioni della PO (si continua ad utilizzare
    // lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
    String url =
        "/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK4";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PARTIALLY_REPORTED.toString()));

    // effettuo la rendicontazione per la seconda delle 2 transazioni della PO (si continua ad
    // utilizzare lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK4/transfers/5/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato sia passato in PO_REPORTED
    url =
        "/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK4";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero la PO non ancora rendicontata della posizione debitoria e verifico che sia ancora in
    // PAID
    url =
        "/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK5";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // effettuo la rendicontazione per le 2 transazioni della PO ancora in stato PAID (si continua
    // ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    mvc.perform(
            post("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/123456IUVMULTIPLEMOCK5/transfers/5/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato sia passato in PO_REPORTED
    url =
        "/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK5";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia passato in reported
    mvc.perform(
            get("/organizations/REPORT_Multiple_All_Partial_Reported_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.REPORTED.toString()));
  }

  @Test
  void reportTransfer_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata) con una sola PO e
    // isPartialPayment=false
    mvc.perform(
            post("/organizations/REPORT_409_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/REPORT_409_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // recupero l'intera posizione debitoria e verifico lo stato in valid
    mvc.perform(
            get("/organizations/REPORT_409_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()));

    // effettuo la rendicontazione per l'unica transazione della PO ma senza pagamenti in essere
    mvc.perform(
            post("/organizations/REPORT_409_12345678901/paymentoptions/123456IUVMOCK1/transfers/1/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void reportTransfer_Multiple_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/REPORT_Multiple_409_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/REPORT_Multiple_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/REPORT_Multiple_409_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK3/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK3"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/REPORT_Multiple_409_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per l'unica transazione della PO (si continua ad utilizzare lo
    // IUV e non il NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/transfers/3/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_REPORTED
    String url =
        "/organizations/REPORT_Multiple_409_12345678901/paymentoptions/"
            + auxDigit
            + "123456IUVMULTIPLEMOCK3";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // provo a rendicontare nuovamente la transazione già rendicontata (si continua ad utilizzare lo
    // IUV e non il NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_409_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/transfers/3/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void reportTransfer_404() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/REPORT_Multiple_404_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/REPORT_Multiple_404_12345678901/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/REPORT_Multiple_404_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMULTIPLEMOCK3/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMULTIPLEMOCK3"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMULTIPLEMOCK3"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // provo a rendicontare una transazione che non esiste (si continua ad utilizzare lo IUV e non
    // il NAV)
    mvc.perform(
            post("/organizations/REPORT_Multiple_404_12345678901/paymentoptions/123456IUVMULTIPLEMOCK3/transfers/x/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound());
  }

  /** UPDATE PAYMENT OPTION'S NOTIFICATION FEE */
  @Test
  void updateNotificationFee_200() throws Exception {

    PaymentPositionDTO paymentPositionDTO =
        DebtPositionMock.paymentPositionForNotificationUpdateMock1();
    PaymentOptionDTO paymentOptionDTO = paymentPositionDTO.getPaymentOption().get(0);
    TransferDTO transferDTO = paymentOptionDTO.getTransfer().get(0);
    NotificationFeeUpdateModel notificationFeeUpdateModel =
        DebtPositionMock.createNotificationFeeMock(150L);

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenReturn(NodeCheckPositionResponse.builder().outcome("OK").build());

    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/PO200_notificationfee_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/PO200_notificationfee_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO200_notificationfee_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.amount")
                .value(
                    paymentOptionDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].amount")
                .value(transferDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()));

    verify(nodeClient, times(1)).getCheckPosition(any(NodeCheckPositionModel.class));

    // leggo nuovamente la payment option per capire se gli amount sono stati modificati
    // correttamente
    mvc.perform(
            get("/organizations/PO200_notificationfee_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.amount")
                .value(
                    paymentOptionDTO.getAmount()
                        + notificationFeeUpdateModel.getNotificationFee()));

    // aggiorno il valore della notification fee, reimpostandolo a zero ed aspettandomi di ritornare
    // al valore di partenza (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO200_notificationfee_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(0L)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value(paymentOptionDTO.getAmount()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].amount").value(transferDTO.getAmount()));

    verify(nodeClient, times(2)).getCheckPosition(any(NodeCheckPositionModel.class));

    // leggo nuovamente la payment option per capire se gli amount sono stati modificati
    // correttamente
    mvc.perform(
            get("/organizations/PO200_notificationfee_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value(paymentOptionDTO.getAmount()));
  }

  @Test
  void updateNotificationFee_NAV_200() throws Exception {

    PaymentPositionDTO paymentPositionDTO =
        DebtPositionMock.paymentPositionForNotificationUpdateMock1();
    PaymentOptionDTO paymentOptionDTO = paymentPositionDTO.getPaymentOption().get(0);
    TransferDTO transferDTO = paymentOptionDTO.getTransfer().get(0);
    NotificationFeeUpdateModel notificationFeeUpdateModel =
        DebtPositionMock.createNotificationFeeMock(150L);

    // la prima volta il nodo torna eccezione poi ritorna sempre OK
    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenThrow(FeignException.BadRequest.class)
        .thenReturn(NodeCheckPositionResponse.builder().outcome("OK").build());

    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/PO200_notificationfee_NAV_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/PO200_notificationfee_NAV_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO200_notificationfee_NAV_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.amount")
                .value(
                    paymentOptionDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].amount")
                .value(transferDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()));

    verify(nodeClient, times(2)).getCheckPosition(any(NodeCheckPositionModel.class));

    // leggo nuovamente la payment option per capire se gli amount sono stati modificati
    // correttamente
    mvc.perform(
            get("/organizations/PO200_notificationfee_NAV_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.amount")
                .value(
                    paymentOptionDTO.getAmount()
                        + notificationFeeUpdateModel.getNotificationFee()));

    // aggiorno il valore della notification fee, reimpostandolo a zero ed aspettandomi di ritornare
    // al valore di partenza (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO200_notificationfee_NAV_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(0L)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value(paymentOptionDTO.getAmount()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].amount").value(transferDTO.getAmount()));

    verify(nodeClient, times(3)).getCheckPosition(any(NodeCheckPositionModel.class));

    // leggo nuovamente la payment option per capire se gli amount sono stati modificati
    // correttamente
    mvc.perform(
            get("/organizations/PO200_notificationfee_NAV_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value(paymentOptionDTO.getAmount()));
  }

  @Test
  void updateNotificationFee_afterDebtPositionUpdate_200() throws Exception {

    PaymentPositionDTO paymentPositionDTO =
        DebtPositionMock.paymentPositionForNotificationUpdateMock1();
    PaymentOptionDTO paymentOptionDTO = paymentPositionDTO.getPaymentOption().get(0);
    TransferDTO transferDTO = paymentOptionDTO.getTransfer().get(0);
    NotificationFeeUpdateModel notificationFeeUpdateModel =
        DebtPositionMock.createNotificationFeeMock(150L);

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenReturn(NodeCheckPositionResponse.builder().outcome("OK").build());

    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/PO200_notificationfee_afterupdate_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/PO200_notificationfee_afterupdate_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO200_notificationfee_afterupdate_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.amount")
                .value(
                    paymentOptionDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].amount")
                .value(transferDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()));

    long poAmountBeforeUpdate =
        paymentOptionDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee();

    verify(nodeClient, times(1)).getCheckPosition(any(NodeCheckPositionModel.class));

    // leggo nuovamente la payment option per capire se gli amount sono stati modificati
    // correttamente
    mvc.perform(
            get("/organizations/PO200_notificationfee_afterupdate_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value(poAmountBeforeUpdate));

    long newPOAmount = 3500L;
    long poAmountAfterUpdate = newPOAmount + notificationFeeUpdateModel.getNotificationFee();

    // modifico la posizione debitoria per capire se la notification fee viene modificata
    paymentPositionDTO
        .getPaymentOption()
        .get(0)
        .setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(1, ChronoUnit.DAYS));
    paymentPositionDTO.getPaymentOption().get(0).setAmount(newPOAmount);
    paymentPositionDTO.getPaymentOption().get(0).getTransfer().get(0).setAmount(newPOAmount);
    paymentPositionDTO.getPaymentOption().get(0).setNotificationFee(5000L);
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO200_notificationfee_afterupdate_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // leggo nuovamente la payment option per capire se gli amount sono stati modificati
    // correttamente in fase di update e il cambiamento si denota nella PO
    mvc.perform(
            get("/organizations/PO200_notificationfee_afterupdate_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value(poAmountAfterUpdate))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].amount").value(poAmountAfterUpdate));
  }

  @Test
  void updateNotificationFee_zeroNotificationFee_200() throws Exception {

    PaymentPositionDTO paymentPositionDTO =
        DebtPositionMock.paymentPositionForNotificationUpdateMock1();

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenReturn(NodeCheckPositionResponse.builder().outcome("OK").build());

    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/PO400_zeronotificationfee_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO400_zeronotificationfee_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(0L)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    verify(nodeClient, times(1)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_negativeNotificationFee_400() throws Exception {
    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/PO400_notificationfee_negative_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // passo una richiesta errata, con notification fee negativo (si continua ad utilizzare lo IUV e
    // non il NAV)
    String url =
        "/organizations/PO400_notificationfee_negative_12345678901/paymentoptions/123456IUVMOCK1/notificationfee";
    mvc.perform(
            MockMvcRequestBuilders.put(url)
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(-150L)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());

    verify(nodeClient, times(0)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_nullNotificationFee_400() throws Exception {
    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/PO400_notificationfee_null_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // passo una richiesta errata, con notification fee nullo (si continua ad utilizzare lo IUV e
    // non il NAV)
    String url =
        "/organizations/PO400_notificationfee_null_12345678901/paymentoptions/123456IUVMOCK1/notificationfee";
    mvc.perform(
            MockMvcRequestBuilders.put(url)
                .content(TestUtil.toJson(new NotificationFeeUpdateModel()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());

    verify(nodeClient, times(0)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_404() throws Exception {
    String url =
        "/organizations/PO404_notificationfee_12345678901/paymentoptions/123456IUVNOTEXIST/notificationfee";
    mvc.perform(
            MockMvcRequestBuilders.put(url)
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(150L)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound());

    verify(nodeClient, times(0)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_paymentOptionAlreadyPaid_422() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/PO422_notificationfee_paid_12345678901/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/PO422_notificationfee_paid_12345678901/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/PO422_notificationfee_paid_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "123456IUVMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("123456IUVMOCK1"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/PO422_notificationfee_paid_12345678901/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la chiamata ma non posso continuare perche la PD è stata gia pagata (si continua ad
    // utilizzare lo IUV e non il NAV)
    String url =
        "/organizations/PO422_notificationfee_paid_12345678901/paymentoptions/123456IUVMOCK1/notificationfee";
    mvc.perform(
            MockMvcRequestBuilders.put(url)
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(150L)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isUnprocessableEntity());

    verify(nodeClient, times(0)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_noValidTransfer_422() throws Exception {

    PaymentPositionDTO paymentPositionDTO =
        DebtPositionMock.paymentPositionForNotificationUpdateMock1();

    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/PO422_notificationfee_invalidtransfer_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // effettuo la chiamata ma non posso continuare perche non esiste un transfer correlata all'EC
    // in input
    paymentPositionDTO
        .getPaymentOption()
        .get(0)
        .getTransfer()
        .get(0)
        .setOrganizationFiscalCode("hfdkjshfkdha");
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO422_notificationfee_invalidtransfer_12345678901/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO422_notificationfee_invalidtransfer_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(150L)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isUnprocessableEntity());

    verify(nodeClient, times(0)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_209() throws Exception {

    PaymentPositionDTO paymentPositionDTO =
        DebtPositionMock.paymentPositionForNotificationUpdateMock1();
    PaymentOptionDTO paymentOptionDTO = paymentPositionDTO.getPaymentOption().get(0);
    TransferDTO transferDTO = paymentOptionDTO.getTransfer().get(0);
    NotificationFeeUpdateModel notificationFeeUpdateModel =
        DebtPositionMock.createNotificationFeeMock(150L);

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenReturn(NodeCheckPositionResponse.builder().outcome("KO").build());

    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/PO209_notificationfee_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/PO209_notificationfee_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO209_notificationfee_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().is(CustomHttpStatus.IN_PROGRESS.value()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.amount")
                .value(
                    paymentOptionDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].amount")
                .value(transferDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()));

    verify(nodeClient, times(1)).getCheckPosition(any(NodeCheckPositionModel.class));

    // leggo nuovamente la payment option per capire se gli amount sono stati modificati
    // correttamente
    mvc.perform(
            get("/organizations/PO209_notificationfee_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.amount")
                .value(
                    paymentOptionDTO.getAmount()
                        + notificationFeeUpdateModel.getNotificationFee()));
  }

  @Test
  void updateNotificationFee_CheckPositionException_209() throws Exception {

    PaymentPositionDTO paymentPositionDTO =
        DebtPositionMock.paymentPositionForNotificationUpdateMock1();
    PaymentOptionDTO paymentOptionDTO = paymentPositionDTO.getPaymentOption().get(0);
    TransferDTO transferDTO = paymentOptionDTO.getTransfer().get(0);
    NotificationFeeUpdateModel notificationFeeUpdateModel =
        DebtPositionMock.createNotificationFeeMock(150L);

    when(nodeClient.getCheckPosition(any(NodeCheckPositionModel.class)))
        .thenThrow(new RuntimeException("Connection timeout"));

    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/PO209_exception_notificationfee_12345678901/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/PO209_exception_notificationfee_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/PO209_exception_notificationfee_12345678901/paymentoptions/123456IUVMOCK1/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().is(CustomHttpStatus.IN_PROGRESS.value()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.amount")
                .value(
                    paymentOptionDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.transfer[0].amount")
                .value(transferDTO.getAmount() + notificationFeeUpdateModel.getNotificationFee()));

    verify(nodeClient, times(1)).getCheckPosition(any(NodeCheckPositionModel.class));

    // leggo nuovamente la payment option per capire se gli amount sono stati modificati
    // correttamente
    mvc.perform(
            get("/organizations/PO209_exception_notificationfee_12345678901/paymentoptions/"
                    + auxDigit
                    + "123456IUVMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.notificationFee")
                .value(notificationFeeUpdateModel.getNotificationFee()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.amount")
                .value(
                    paymentOptionDTO.getAmount()
                        + notificationFeeUpdateModel.getNotificationFee()));
  }

  /** VALIDATION TEST - unexpected case */
  @Test
  void ValidationError_checkPaymentPositionOpen() throws Exception {
    try {
      PaymentOption localMockPO = new PaymentOption();
      PaymentPosition localMockPP = new PaymentPosition();
      localMockPP.setStatus(DebtPositionStatus.VALID);
      localMockPO.setStatus(PaymentOptionStatus.PO_PAID);
      localMockPO.setIsPartialPayment(false);
      localMockPP.addPaymentOption(localMockPO);
      DebtPositionValidation.checkPaymentPositionPayability(localMockPP, "mockIUV");
    } catch (AppException e) {
      assertTrue(true);
    } catch (Exception e) {
      fail("Not the expected exception: " + e.getMessage());
    }
  }

  @Test
  void ValidationError_checkPaymentPositionAccountability_PO() throws Exception {
    try {
      PaymentOption localMockPO = new PaymentOption();
      PaymentPosition localMockPP = new PaymentPosition();
      localMockPP.setStatus(DebtPositionStatus.PAID);
      localMockPO.setStatus(PaymentOptionStatus.PO_PAID);
      localMockPO.setIsPartialPayment(false);
      localMockPO.setIuv("iuv");
      localMockPP.addPaymentOption(localMockPO);
      DebtPositionValidation.checkPaymentPositionAccountability(localMockPP, "mockIUV", "mockTxID");
    } catch (AppException e) {
      assertTrue(true);
    } catch (Exception e) {
      fail("Not the expected exception: " + e.getMessage());
    }
  }

  @Test
  void ValidationError_checkPaymentPositionAccountability_Transfer() throws Exception {
    try {
      PaymentOption localMockPO = new PaymentOption();
      PaymentPosition localMockPP = new PaymentPosition();
      Transfer localTransfer = new Transfer();
      localMockPP.setStatus(DebtPositionStatus.PAID);
      localMockPO.setStatus(PaymentOptionStatus.PO_PAID);
      localTransfer.setIdTransfer("1");
      localMockPO.addTransfer(localTransfer);
      localMockPO.setIsPartialPayment(false);
      localMockPO.setIuv("mockIUV");
      localMockPP.addPaymentOption(localMockPO);
      DebtPositionValidation.checkPaymentPositionAccountability(localMockPP, "mockIUV", "mockTxID");
    } catch (AppException e) {
      assertTrue(true);
    } catch (Exception e) {
      fail("Not the expected exception: " + e.getMessage());
    }
  }
}
