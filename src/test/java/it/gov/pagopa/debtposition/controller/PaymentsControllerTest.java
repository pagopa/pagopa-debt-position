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
import java.time.ZonedDateTime;
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
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.bean.override.mockito.MockitoSpyBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class PaymentsControllerTest {
	
  @Autowired private ObjectMapper objectMapper;

  @Autowired private MockMvc mvc;

  @Mock private ModelMapper modelMapperMock;

  @MockitoBean private NodeClient nodeClient;

  @MockitoBean private SendClient sendClient;

  @MockitoSpyBean private PaymentsService paymentsService;

  @Value("${nav.aux.digit}")
  private String auxDigit;

  @BeforeEach
  void setUp() {}

  /** GET PAYMENT OPTION BY NAV */
  @Test
  void getPaymentOptionByNAV_200() throws Exception {
    // creo una posizione debitoria con NAV custom e recupero la payment option associata
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    pp.getPaymentOption().forEach(po -> po.setNav("111" + po.getIuv()));
    mvc.perform(
            post("/organizations/200123456789011/debtpositions")
                .content(TestUtil.toJson(pp, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            result -> {
              int status = result.getResponse().getStatus();
              if (status != HttpStatus.CREATED.value() && status != HttpStatus.CONFLICT.value()) {
                throw new AssertionError(
                    "Expected status 201 (Created) or 409 (Conflict), but got: " + status);
              }
            });

    NotificationPriceResponse priceRes =
        new NotificationPriceResponse(
            "IUN", 1, 1, 0, 0, ZonedDateTime.now(), ZonedDateTime.now(), 1, 1);
    when(sendClient.getNotificationFee(anyString(), anyString())).thenReturn(priceRes);

    String url = "/organizations/200123456789011/paymentoptions/1234561";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value("1111234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"));
  }

  @Test
  void getPaymentOptionByNAVSendSyncTimeoutException_200() throws Exception {
    // creo una posizione debitoria con NAV custom e recupero la payment option associata
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    String organization = "200123456789012";

    for (PaymentOptionDTO po : pp.getPaymentOption()) {
      po.setNav(po.getIuv());
    }

    mvc.perform(
        post("/organizations/" + organization + "/debtpositions")
            .content(TestUtil.toJson(pp, objectMapper))
            .contentType(MediaType.APPLICATION_JSON));

    for (PaymentOptionDTO po : pp.getPaymentOption()) {
      ArrayList<Notice> notices = new ArrayList<>();
      notices.add(new Notice(organization, po.getNav()));
      mvc.perform(
              post("/internal/config/send")
                  .contentType(MediaType.APPLICATION_JSON)
                  .content(objectMapper.writeValueAsString(notices)))
          .andExpect(status().isOk());
    }

    when(sendClient.getNotificationFee(anyString(), anyString()))
        .thenThrow(new RuntimeException("Connection timeout"));

    String url = "/organizations/" + organization + "/paymentoptions/1234561";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value("1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"));
  }

  @Test
  void getPaymentOptionByNAVSendSyncOk_200() throws Exception {
    // creo una posizione debitoria con NAV custom e recupero la payment option associata
    PaymentPositionDTO pp = DebtPositionMock.getMock1();
    String organization = "123456789013";

    for (PaymentOptionDTO po : pp.getPaymentOption()) {
      po.setNav(po.getIuv());
    }

    mvc.perform(
        post("/organizations/" + organization + "/debtpositions")
            .content(TestUtil.toJson(pp, objectMapper))
            .contentType(MediaType.APPLICATION_JSON));

    for (PaymentOptionDTO po : pp.getPaymentOption()) {
      ArrayList<Notice> notices = new ArrayList<>();
      notices.add(new Notice(organization, po.getNav()));
      mvc.perform(
              post("/internal/config/send")
                  .contentType(MediaType.APPLICATION_JSON)
                  .content(objectMapper.writeValueAsString(notices)))
          .andExpect(status().isOk());
    }

    long firstPOAmount = pp.getPaymentOption().get(0).getAmount();
    String firstPONav = pp.getPaymentOption().get(0).getNav();
    NotificationPriceResponse priceRes =
        new NotificationPriceResponse(
            "IUN", 1, 1, 0, 0, ZonedDateTime.now(), ZonedDateTime.now(), 1, 1);
    Integer price = priceRes.getTotalPrice();
    when(sendClient.getNotificationFee(anyString(), anyString())).thenReturn(priceRes);

    // Get first Payment Option
    String url = "/organizations/" + organization + "/paymentoptions/" + firstPONav;
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value("1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value(firstPOAmount + price));
  }

  @Test
  void getPaymentOptionByNAV_MultiplePO_200() throws Exception {
    // creo una posizione debitoria con NAV custom e con più payment option associate
    PaymentPositionDTO pp = DebtPositionMock.getMock2();
    pp.getPaymentOption().forEach(po -> po.setNav(po.getIuv()));
    mvc.perform(
            post("/organizations/200123456789014/debtpositions")
                .content(TestUtil.toJson(pp, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // ne recupero una e verifico sia quella attesa
    String url =
        "/organizations/200123456789014/paymentoptions/1234562";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value("1234562"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234562"))
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
            post("/organizations/123456789015/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/123456789015/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid e il nav valorizzato nel
    // formato <AUX_DIGIT>+IUV
    mvc.perform(
            post("/organizations/123456789015/paymentoptions/"
                    + auxDigit
                    + "1234561/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero la payment option e verifico di nuovo lo stato in paid e il nav valorizzato nel
    // formato <AUX_DIGIT>+IUV
    String url = "/organizations/123456789015/paymentoptions/" + auxDigit + "1234561";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iupd").value("12345678901IUPDMOCK1"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.amount").value("1000"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.transfer[*]").value(Matchers.hasSize(1)));
  }

  @Test
  void getPaymentOptionByNAV_404() throws Exception {
    String url = "/organizations/200123456789016/paymentoptions/123456";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void getPaymentOptionWithMetadataByNAV_POPAID_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/123456789017/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMetadataMock8(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234569"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].iuv")
                .value("1234569"))
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
            post("/organizations/123456789017/debtpositions/12345678901IUPDMETADATAMOCK7/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/123456789017/paymentoptions/"
                    + auxDigit
                    + "1234569/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234569"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234569"))
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
        "/organizations/123456789017/paymentoptions/" + auxDigit + "1234569";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234569"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234569"))
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
            post("/organizations/123456789017/paymentoptions/1234569/transfers/3/report")
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
            post("/organizations/123456789018/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].iuv").value("1234561"));

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/123456789018/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/123456789018/paymentoptions/" + auxDigit + "1234561/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/123456789018/debtpositions/12345678901IUPDMOCK1")
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
            post("/organizations/12345678919/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/12345678919/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    PaymentOptionDTO data = DebtPositionMock.getPayPOMock1();
    data.setPaymentDate(null);
    data.setPaymentMethod(null);
    data.setFee(0); // because it is a long initialized to 0
    mvc.perform(
            post("/organizations/12345678919/paymentoptions/" + auxDigit + "1234561/pay")
                .content(TestUtil.toJson(data, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/12345678919/debtpositions/12345678901IUPDMOCK1")
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
            post("/organizations/1234567890111/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/1234567890111/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/1234567890111/paymentoptions/"
                    + auxDigit
                    + "1234563/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234563"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234563"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/1234567890111/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].nav")
                .value(auxDigit + "1234563"));
  }

  @Test
  void payPaymentOption_Multiple_Partial_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/1234567890122/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/1234567890122/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/1234567890122/paymentoptions/"
                    + auxDigit
                    + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234564"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234564"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/1234567890122/debtpositions/12345678901IUPDMULTIPLEMOCK2")
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
            post("/organizations/1234567890133/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/1234567890133/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/1234567890133/paymentoptions/"
                    + auxDigit
                    + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234564"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234564"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/1234567890133/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo la notifica di pagamento della seconda rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/1234567890133/paymentoptions/"
                    + auxDigit
                    + "1234565/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234565"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234565"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
    mvc.perform(
            get("/organizations/1234567890133/debtpositions/12345678901IUPDMULTIPLEMOCK2")
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
            post("/organizations/1234567890144/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/1234567890144/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/1234567890144/paymentoptions/"
                    + auxDigit
                    + "1234563/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234563"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234563"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/1234567890144/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo un nuovo pagamento per la stessa payment option
    mvc.perform(
            post("/organizations/1234567890144/paymentoptions/"
                    + auxDigit
                    + "1234563/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void payPaymentOption_Multiple_Partial_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/1234567890155/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/1234567890155/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/1234567890155/paymentoptions/"
                    + auxDigit
                    + "1234563/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234563"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234563"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/1234567890155/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo un nuovo pagamento su una delle rate parziali (setIsPartialPayment = true) per la
    // payment option
    mvc.perform(
            post("/organizations/1234567890155/paymentoptions/"
                    + auxDigit
                    + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void getPaymentOption_Multiple_Partial_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/1234567890166/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/1234567890166/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (isPartialPayment = true) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/1234567890166/paymentoptions/"
                    + auxDigit
                    + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234564"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234564"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/1234567890166/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo una GET/ACTIVATE sulla payment option corrispondente alla rata unica/intero importo
    // e ottengo errore 409
    mvc.perform(
            get("/organizations/1234567890166/paymentoptions/"
                    + auxDigit
                    + "1234563")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void payPaymentOption_ok() throws Exception {
    // creo una posizione debitoria (con 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/4221234567890177/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock6(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/4221234567890177/debtpositions/12345678901IUPDMOCK5/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // l'errore 422 di 'Not in payable state'
    mvc.perform(
            post("/organizations/4221234567890177/paymentoptions/"
                    + auxDigit
                    + "1234566/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

  @Test
  void payPaymentOption_404() throws Exception {
    // provo a pagare una payment option che non esiste
    String url = "/organizations/4001234567890188/paymentoptions/123456";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void payPaymentOption_400() throws Exception {
    // provo a pagare una payment option con body della request non corretto
    mvc.perform(
            post("/organizations/4091234567890199/paymentoptions/"
                    + auxDigit
                    + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPO400Mock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void markAsPaidPaymentOption_ok() throws Exception {
    // creo una posizione debitoria (con 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/20012345678901000/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock10(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/20012345678901000/debtpositions/12345678901IUPDMOCK10_markd/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo l'aggiornamento della posizione debutoria come già pagata e verifico
    // l'errore 422 di 'Not in payable state'
    mvc.perform(
            post("/organizations/20012345678901000/paymentoptions/paids/"
                    + auxDigit
                    + "12345610")
                .content("{}")
                .queryParam("segregationCodes", "12")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

  @Test
  void markAsPaidPaymentOption_ok_withBody() throws Exception {
    // creo una posizione debitoria (con 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/42212345678901111/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock10(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/42212345678901111/debtpositions/12345678901IUPDMOCK10_markd/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo l'aggiornamento della posizione debutoria come già pagata e verifico
    // l'errore 422 di 'Not in payable state'
    mvc.perform(
            post("/organizations/42212345678901111/paymentoptions/paids/"
                    + auxDigit
                    + "12345610")
                .content("{\"paymentDate\":\"2025-01-01T10:00:00.000Z\"}")
                .queryParam("segregationCodes", "12")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

  @Test
  void markAsPaidPaymentOption_404() throws Exception {
    // provo a pagare una payment option che non esiste
    String url = "/organizations/40012345678901222/paymentoptions/paids/3123456";
    mvc.perform(
            post(url)
                .queryParam("segregationCodes", "12")
                .content("{\"paymentDate\":\"2025-01-01T10:00:00.000Z\"}")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void markAsPaidPaymentOption_403() throws Exception {
    // provo a pagare una payment option su cui non ho i permessi di accedere
    String url =
        "/organizations/42212345678901333/paymentoptions/paids/" + auxDigit + "12345610";
    mvc.perform(
            post(url)
                .queryParam("segregationCodes", "51")
                .content("{}")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isForbidden())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  /** REPORT A TRANSFER */
  @Test
  void reportTransfer_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con una
    // sola PO e isPartialPayment=false
    mvc.perform(
            post("/organizations/20012345678901444/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/20012345678901444/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/20012345678901444/paymentoptions/"
                    + auxDigit
                    + "1234561/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/20012345678901444/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per l'unica transazione della PO (si continua ad utilizzare lo
    // IUV e non il NAV)
    mvc.perform(
            post("/organizations/20012345678901444/paymentoptions/1234561/transfers/1/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_REPORTED
    String url =
        "/organizations/20012345678901444/paymentoptions/" + auxDigit + "1234561";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in reported
    mvc.perform(
            get("/organizations/20012345678901444/debtpositions/12345678901IUPDMOCK1")
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
            post("/organizations/12345678901555/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/12345678901555/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/12345678901555/paymentoptions/"
                    + auxDigit
                    + "1234563/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234563"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234563"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/12345678901555/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per l'unica transazione della PO (si continua ad utilizzare lo
    // IUV e non il NAV)
    mvc.perform(
            post("/organizations/12345678901555/paymentoptions/1234563/transfers/3/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_REPORTED
    String url =
        "/organizations/12345678901555/paymentoptions/"
            + auxDigit
            + "1234563";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in reported
    mvc.perform(
            get("/organizations/12345678901555/debtpositions/12345678901IUPDMULTIPLEMOCK2")
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
            post("/organizations/12345678901666/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/12345678901666/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/12345678901666/paymentoptions/"
                    + auxDigit
                    + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234564"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234564"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/12345678901666/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo la rendicontazione per la transazione (si continua ad utilizzare lo IUV e non il
    // NAV)
    mvc.perform(
            post("/organizations/12345678901666/paymentoptions/1234564/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
    String url =
        "/organizations/12345678901666/paymentoptions/"
            + auxDigit
            + "1234564";
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
            post("/organizations/12345678901777/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/12345678901777/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/12345678901777/paymentoptions/"
                    + auxDigit
                    + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234564"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234564"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/12345678901777/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo la notifica di pagamento della seconda rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/12345678901777/paymentoptions/"
                    + auxDigit
                    + "1234565/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234565"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234565"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
    mvc.perform(
            get("/organizations/12345678901777/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per una delle 2 transazioni della PO (si continua ad utilizzare
    // lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/12345678901777/paymentoptions/1234564/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
    String url =
        "/organizations/12345678901777/paymentoptions/"
            + auxDigit
            + "1234564";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PARTIALLY_REPORTED.toString()));

    // effettuo la rendicontazione per la seconda delle 2 transazioni della PO (si continua ad
    // utilizzare lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/12345678901777/paymentoptions/1234564/transfers/5/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato sia passato in PO_REPORTED
    url =
        "/organizations/12345678901777/paymentoptions/"
            + auxDigit
            + "1234564";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero la PO non ancora rendicontata della posizione debitoria e verifico che sia ancora in
    // PAID
    url =
        "/organizations/12345678901777/paymentoptions/"
            + auxDigit
            + "1234565";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia ancora in paid
    mvc.perform(
            get("/organizations/12345678901777/debtpositions/12345678901IUPDMULTIPLEMOCK2")
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
            post("/organizations/12345678901888/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/12345678901888/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento di una rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/12345678901888/paymentoptions/"
                    + auxDigit
                    + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234564"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234564"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in partially paid
    mvc.perform(
            get("/organizations/12345678901888/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PARTIALLY_PAID.toString()));

    // effettuo la notifica di pagamento della seconda rata parziale (setIsPartialPayment = true) e
    // verifico lo stato in paid
    mvc.perform(
            post("/organizations/12345678901888/paymentoptions/"
                    + auxDigit
                    + "1234565/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234565"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234565"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia passato in paid
    mvc.perform(
            get("/organizations/12345678901888/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per una delle 2 transazioni della PO (si continua ad utilizzare
    // lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/12345678901888/paymentoptions/1234564/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_PARTIALLY_REPORTED
    String url =
        "/organizations/12345678901888/paymentoptions/"
            + auxDigit
            + "1234564";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PARTIALLY_REPORTED.toString()));

    // effettuo la rendicontazione per la seconda delle 2 transazioni della PO (si continua ad
    // utilizzare lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/12345678901888/paymentoptions/1234564/transfers/5/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato sia passato in PO_REPORTED
    url =
        "/organizations/12345678901888/paymentoptions/"
            + auxDigit
            + "1234564";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero la PO non ancora rendicontata della posizione debitoria e verifico che sia ancora in
    // PAID
    url =
        "/organizations/12345678901888/paymentoptions/"
            + auxDigit
            + "1234565";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // effettuo la rendicontazione per le 2 transazioni della PO ancora in stato PAID (si continua
    // ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            post("/organizations/12345678901888/paymentoptions/1234565/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    mvc.perform(
            post("/organizations/12345678901888/paymentoptions/1234565/transfers/5/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato sia passato in PO_REPORTED
    url =
        "/organizations/12345678901888/paymentoptions/"
            + auxDigit
            + "1234565";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero l'intera posizione debitoria e verifico che lo stato sia passato in reported
    mvc.perform(
            get("/organizations/12345678901888/debtpositions/12345678901IUPDMULTIPLEMOCK2")
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
            post("/organizations/12345678901999/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/12345678901999/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // recupero l'intera posizione debitoria e verifico lo stato in valid
    mvc.perform(
            get("/organizations/12345678901999/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()));

    // effettuo la rendicontazione per l'unica transazione della PO ma senza pagamenti in essere
    mvc.perform(
            post("/organizations/12345678901999/paymentoptions/1234561/transfers/1/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void reportTransfer_Multiple_409() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/123456789010000/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/123456789010000/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/123456789010000/paymentoptions/"
                    + auxDigit
                    + "1234563/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234563"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234563"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/123456789010000/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la rendicontazione per l'unica transazione della PO (si continua ad utilizzare lo
    // IUV e non il NAV)
    mvc.perform(
            post("/organizations/123456789010000/paymentoptions/1234563/transfers/3/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_REPORTED
    String url =
        "/organizations/123456789010000/paymentoptions/"
            + auxDigit
            + "1234563";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // provo a rendicontare nuovamente la transazione già rendicontata (si continua ad utilizzare lo
    // IUV e non il NAV)
    mvc.perform(
            post("/organizations/123456789010000/paymentoptions/1234563/transfers/3/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  @Test
  void reportTransfer_404() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con più
    // opzioni di pagamento
    mvc.perform(
            post("/organizations/123456789011111/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/123456789011111/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento della rata unica (setIsPartialPayment = false) e verifico
    // lo stato in paid
    mvc.perform(
            post("/organizations/123456789011111/paymentoptions/"
                    + auxDigit
                    + "1234563/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234563"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234563"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // provo a rendicontare una transazione che non esiste (si continua ad utilizzare lo IUV e non
    // il NAV)
    mvc.perform(
            post("/organizations/123456789011111/paymentoptions/123456IUVMULTIPLEMOCK3/transfers/x/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound());
  }

  @Test
  void reportTransfer_ACA_200() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato) con una
    // sola PO e isPartialPayment=false
    PaymentPositionDTO mock1 = DebtPositionMock.getMock1();
    mvc.perform(
            post("/organizations/123456789012222/debtpositions?serviceType=ACA")
                .content(TestUtil.toJson(mock1, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/123456789012222/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/123456789012222/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()));

    // effettuo la rendicontazione per l'unica transazione della PO (si continua ad utilizzare lo
    // IUV e non il NAV)
    mvc.perform(
            post("/organizations/123456789012222/paymentoptions/1234561/transfers/1/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(TransferStatus.T_REPORTED.toString()));

    // recupero la PO e verifico lo stato in PO_REPORTED
    String url =
        "/organizations/123456789012222/paymentoptions/" + auxDigit + "1234561";
    mvc.perform(get(url).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_REPORTED.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in reported
    mvc.perform(
            get("/organizations/123456789012222/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.REPORTED.toString()));
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
            post("/organizations/20012345678901333/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/20012345678901333/paymentoptions/"
                    + auxDigit
                    + "1234561")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/20012345678901333/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel, objectMapper))
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
            get("/organizations/20012345678901333/paymentoptions/"
                    + auxDigit
                    + "1234561")
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
                    "/organizations/20012345678901333/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(0L), objectMapper))
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
            get("/organizations/20012345678901333/paymentoptions/"
                    + auxDigit
                    + "1234561")
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
            post("/organizations/200123456789014444/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/200123456789014444/paymentoptions/"
                    + auxDigit
                    + "1234561")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/200123456789014444/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel, objectMapper))
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
            get("/organizations/200123456789014444/paymentoptions/"
                    + auxDigit
                    + "1234561")
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
                    "/organizations/200123456789014444/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(0L), objectMapper))
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
            get("/organizations/200123456789014444/paymentoptions/"
                    + auxDigit
                    + "1234561")
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
            post("/organizations/20012345678901555/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/20012345678901555/paymentoptions/"
                    + auxDigit
                    + "1234561")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/20012345678901555/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel, objectMapper))
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
            get("/organizations/20012345678901555/paymentoptions/"
                    + auxDigit
                    + "1234561")
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
                    "/organizations/20012345678901555/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(paymentPositionDTO, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // leggo nuovamente la payment option per capire se gli amount sono stati modificati
    // correttamente in fase di update e il cambiamento si denota nella PO
    mvc.perform(
            get("/organizations/20012345678901555/paymentoptions/"
                    + auxDigit
                    + "1234561")
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
            post("/organizations/40012345678901666/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/40012345678901666/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(0L), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    verify(nodeClient, times(1)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_negativeNotificationFee_400() throws Exception {
    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/40012345678901777/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // passo una richiesta errata, con notification fee negativo (si continua ad utilizzare lo IUV e
    // non il NAV)
    String url =
        "/organizations/40012345678901777/paymentoptions/1234561/notificationfee";
    mvc.perform(
            MockMvcRequestBuilders.put(url)
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(-150L), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());

    verify(nodeClient, times(0)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_nullNotificationFee_400() throws Exception {
    // creo una posizione debitoria e recupero la payment option associata
    mvc.perform(
            post("/organizations/40012345678901888/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // passo una richiesta errata, con notification fee nullo (si continua ad utilizzare lo IUV e
    // non il NAV)
    String url =
        "/organizations/40012345678901888/paymentoptions/1234561/notificationfee";
    mvc.perform(
            MockMvcRequestBuilders.put(url)
                .content(TestUtil.toJson(new NotificationFeeUpdateModel(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());

    verify(nodeClient, times(0)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_404() throws Exception {
    String url =
        "/organizations/40412345678901999/paymentoptions/123456/notificationfee";
    mvc.perform(
            MockMvcRequestBuilders.put(url)
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(150L), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound());

    verify(nodeClient, times(0)).getCheckPosition(any(NodeCheckPositionModel.class));
  }

  @Test
  void updateNotificationFee_paymentOptionAlreadyPaid_422() throws Exception {
    // creo una posizione debitoria (senza 'validity date' impostata e nav non valorizzato)
    mvc.perform(
            post("/organizations/422123456789010000/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata/validata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/422123456789010000/debtpositions/12345678901IUPDMOCK1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // effettuo la notifica di pagamento e verifico lo stato in paid
    mvc.perform(
            post("/organizations/422123456789010000/paymentoptions/"
                    + auxDigit
                    + "1234561/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.nav").value(auxDigit + "1234561"))
        .andExpect(MockMvcResultMatchers.jsonPath("$.iuv").value("1234561"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(PaymentOptionStatus.PO_PAID.toString()));

    // recupero l'intera posizione debitoria e verifico lo stato in paid
    mvc.perform(
            get("/organizations/422123456789010000/debtpositions/12345678901IUPDMOCK1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.PAID.toString()));

    // effettuo la chiamata ma non posso continuare perche la PD è stata gia pagata (si continua ad
    // utilizzare lo IUV e non il NAV)
    String url =
        "/organizations/422123456789010000/paymentoptions/1234561/notificationfee";
    mvc.perform(
            MockMvcRequestBuilders.put(url)
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(150L), objectMapper))
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
            post("/organizations/422123456789011111/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/422123456789011111/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(DebtPositionMock.createNotificationFeeMock(150L), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().is(209));

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
                    "/organizations/422123456789011111/debtpositions/12345678901IUPDMOCK1")
                .content(TestUtil.toJson(paymentPositionDTO, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isUnprocessableEntity());
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
            post("/organizations/209123456789012222/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/209123456789012222/paymentoptions/"
                    + auxDigit
                    + "1234561")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/209123456789012222/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel, objectMapper))
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
            get("/organizations/209123456789012222/paymentoptions/"
                    + auxDigit
                    + "1234561")
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
            post("/organizations/209123456789013333/debtpositions")
                .content(TestUtil.toJson(paymentPositionDTO, objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // leggo il valore della notification fee per accertarmi che non venga inserita anche se passata
    // in input
    mvc.perform(
            get("/organizations/209123456789013333/paymentoptions/"
                    + auxDigit
                    + "1234561")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.notificationFee").value(0L));

    // aggiorno il valore della notification fee (si continua ad utilizzare lo IUV e non il NAV)
    mvc.perform(
            MockMvcRequestBuilders.put(
                    "/organizations/209123456789013333/paymentoptions/1234561/notificationfee")
                .content(TestUtil.toJson(notificationFeeUpdateModel, objectMapper))
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
            get("/organizations/209123456789013333/paymentoptions/"
                    + auxDigit
                    + "1234561")
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
  void recomputeStatusV3_planMixedPaidAndReported_resultsInPP_PAID() throws Exception {
    // 1) A PD with a 2-payment plan is created (plan + single option) and published
    mvc.perform(
            post("/organizations/555123456789000/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post("/organizations/555123456789000/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // 2) Both installments of the plan are paid (1234564 and 1234565)
    mvc.perform(
            post("/organizations/555123456789000/paymentoptions/" + auxDigit + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.status")
            .value(PaymentOptionStatus.PO_PAID.toString()));

    mvc.perform(
            post("/organizations/555123456789000/paymentoptions/" + auxDigit + "1234565/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.status")
            .value(PaymentOptionStatus.PO_PAID.toString()));

    // 3) PP = PAID is verified after ALL plan installments have been paid
    mvc.perform(
            get("/organizations/555123456789000/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.status")
            .value(DebtPositionStatus.PAID.toString()));

    // 4) ONLY one of the two installments is reported (1234564) -> PO_REPORTED, the other remains PO_PAID
    mvc.perform(
            post("/organizations/555123456789000/paymentoptions/1234564/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.status")
            .value(TransferStatus.T_REPORTED.toString()));

    mvc.perform(
            post("/organizations/555123456789000/paymentoptions/1234564/transfers/5/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.status")
            .value(TransferStatus.T_REPORTED.toString()));

    // 5) The plan is in a mixed condition: { PO_REPORTED (1234564), PO_PAID (1234565) } -> PP MUST remain PAID
    mvc.perform(
            get("/organizations/555123456789000/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.status")
            .value(DebtPositionStatus.PAID.toString()));
  }

  @Test
  void recomputeStatusV3_planAllReported_resultsInPP_REPORTED() throws Exception {
    // 1) A PD with a 2-payment plan is created (plan + single option) and published
    mvc.perform(
            post("/organizations/556123456789000/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post("/organizations/556123456789000/debtpositions/12345678901IUPDMULTIPLEMOCK2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // 2) Both installments of the plan are paid (1234564 and 1234565)
    mvc.perform(
            post("/organizations/556123456789000/paymentoptions/" + auxDigit + "1234564/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    mvc.perform(
            post("/organizations/556123456789000/paymentoptions/" + auxDigit + "1234565/pay")
                .content(TestUtil.toJson(DebtPositionMock.getPayPOMock1(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // 3) Both installments are brought to REPORTED
    mvc.perform(
            post("/organizations/556123456789000/paymentoptions/1234564/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    mvc.perform(
            post("/organizations/556123456789000/paymentoptions/1234564/transfers/5/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    mvc.perform(
            post("/organizations/556123456789000/paymentoptions/1234565/transfers/4/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    mvc.perform(
            post("/organizations/556123456789000/paymentoptions/1234565/transfers/5/report")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // 4) All plan installments are REPORTED -> PP = REPORTED
    mvc.perform(
            get("/organizations/556123456789000/debtpositions/12345678901IUPDMULTIPLEMOCK2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.status")
            .value(DebtPositionStatus.REPORTED.toString()));
  }
  
  //========================= NEW TESTS FOR verifyPaymentOptions API =========================

  @Test
  void verifyPaymentOptions_single_200() throws Exception {
	  String organization = "700123456789000";
	  mvc.perform(
			  post("/organizations/" + organization + "/debtpositions")
			  .content(TestUtil.toJson(DebtPositionMock.getMock1(), objectMapper))
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isCreated());

	  String navOrIuv = "1234561";

	  mvc.perform(
			  post("/payment-options/organizations/{organizationfiscalcode}/notices/{nav}", organization, navOrIuv)
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isOk())
	  .andExpect(content().contentType(MediaType.APPLICATION_JSON))
	  .andExpect(MockMvcResultMatchers.jsonPath("$.organizationFiscalCode").value(organization))
	  // Only 1 group expected (single)
	  .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOptions.length()").value(1))
	  .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOptions[0].numberOfInstallments").value(1))
	  .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOptions[0].description").value("Payment in a single installment"));
  }
  
  @Test
  void verifyPaymentOptions_grouping_singleAndPlan_200_and_ordering() throws Exception {
    String organization = "700123456789001";
    mvc.perform(
            post("/organizations/" + organization + "/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMock3(), objectMapper))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String navOrIuv = "1234563";

    mvc.perform(
            post("/payment-options/organizations/{organizationfiscalcode}/notices/{nav}", organization, navOrIuv)
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.paymentOptions.length()",
                Matchers.greaterThanOrEqualTo(2)))
        // there must be AT LEAST one single group
        .andExpect(MockMvcResultMatchers.jsonPath(
                "$.paymentOptions[*].numberOfInstallments",
                Matchers.hasItem(1)))
        // there must be AT LEAST one plan group (>=2 installments)
        .andExpect(MockMvcResultMatchers.jsonPath(
                "$.paymentOptions[*].numberOfInstallments",
                Matchers.hasItem(Matchers.greaterThanOrEqualTo(2))))
        .andExpect(MockMvcResultMatchers.jsonPath(
                "$.paymentOptions[*].description",
                Matchers.hasItem("Payment in a single installment")))
        .andExpect(MockMvcResultMatchers.jsonPath(
                "$.paymentOptions[*].description",
                Matchers.hasItem(Matchers.startsWith("Installment plan of"))));
  }



  @Test
  void verifyPaymentOptions_404_whenNotFound() throws Exception {
	  String organization = "700123456789002";
	  String navOrIuv = "99999999"; // non-existent

	  mvc.perform(
			  post("/payment-options/organizations/{organizationfiscalcode}/notices/{nav}", organization, navOrIuv)
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isNotFound())
	  .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void verifyPaymentOptions_400_onMalformedNAV() throws Exception {
	  // Non-numeric NAV --> @Pattern("^\\d{1,30}$") is violated
	  String organization = "700123456789003";
	  String badNav = "ABCDEF";

	  mvc.perform(
			  post("/payment-options/organizations/{organizationfiscalcode}/notices/{nav}", organization, badNav)
			  .contentType(MediaType.APPLICATION_JSON))
	  .andExpect(status().isBadRequest())
	  .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_PROBLEM_JSON_VALUE))
	  .andExpect(MockMvcResultMatchers.jsonPath("$.status").value(400))
	  .andExpect(MockMvcResultMatchers.jsonPath("$.title").exists())
	  .andExpect(MockMvcResultMatchers.jsonPath("$.detail").exists());
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
