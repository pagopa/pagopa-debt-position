package it.gov.pagopa.debtposition.controller;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.TransferMetadataModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.apache.commons.lang3.RandomStringUtils;
import org.hamcrest.core.IsNull;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class DebtPositionControllerV3Test {
  @Autowired private MockMvc mvc;

  @Mock private ModelMapper modelMapperMock;

  private static final String ORG_FISCAL_CODE = "7777777777";

  @Test
  void getDebtPositionByIUPD_200_1() throws Exception {
    this.getDebtPositionByIUPD_200(1, 1);
  }

  @Test
  void getDebtPositionByIUPD_200_2() throws Exception {
    this.getDebtPositionByIUPD_200(1, 2);
  }

  @Test
  void getDebtPositionByIUPD_200_3() throws Exception {
    this.getDebtPositionByIUPD_200(2, 1);
  }

  private void getDebtPositionByIUPD_200(int numberOfPO, int numberOfInstallment) throws Exception {
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    PaymentPositionModelV3 paymentPositionV3 =
        createPaymentPositionV3(numberOfPO, numberOfInstallment);

    mvc.perform(
            post(uri)
                .content(TestUtil.toJson(paymentPositionV3))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String getPositionUri =
        String.format(
            "/v3/organizations/%s/debtpositions/%s", ORG_FISCAL_CODE, paymentPositionV3.getIupd());
    mvc.perform(get(getPositionUri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void getDebtPositionList() throws Exception {
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    PaymentPositionModelV3 paymentPosition1 = createPaymentPositionV3(1, 1);
    PaymentPositionModelV3 paymentPosition2 = createPaymentPositionV3(1, 1);

    mvc.perform(
            post(uri)
                .content(TestUtil.toJson(paymentPosition1))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post(uri)
                .content(TestUtil.toJson(paymentPosition2))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    String getPositionUri =
        String.format("/v3/organizations/%s/debtpositions?page=0", ORG_FISCAL_CODE)
            + "&due_date_from="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plusDays(50))
            + "&due_date_to="
            + df.format(LocalDateTime.now(ZoneOffset.UTC).plusDays(70))
            + "&orderby=IUPD&ordering=ASC";
    mvc.perform(get(getPositionUri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            jsonPath(
                "$.payment_position_list[*].iupd",
                hasItem(containsString(paymentPosition1.getIupd()))))
        .andExpect(
            jsonPath(
                "$.payment_position_list[*].iupd",
                hasItem(containsString(paymentPosition2.getIupd()))));
  }

  @Test
  void createDebtPosition_201_1() throws Exception {
    this.createDebtPosition_201(1, 1);
  }

  @Test
  void createDebtPosition_201_2() throws Exception {
    this.createDebtPosition_201(2, 1);
  }

  @Test
  void createDebtPosition_201_3() throws Exception {
    this.createDebtPosition_201(1, 2);
  }

  void createDebtPosition_201(int numberOfPO, int numberOfInstallment) throws Exception {
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    mvc.perform(
            post(uri)
                .content(TestUtil.toJson(createPaymentPositionV3(numberOfPO, numberOfInstallment)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(jsonPath("$.paymentOption[0].installments[0].iuv").isNotEmpty())
        .andExpect(jsonPath("$.paymentOption[0].installments[0].nav").isNotEmpty())
        .andExpect(
            jsonPath("$.paymentOption[0].installments[0].transfer[0].companyName")
                .value("CompanyName"))
        .andExpect(jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(jsonPath("$.paymentOption[0].validityDate").value(IsNull.nullValue()));
  }

  @Test
  void createDebtPosition_OkMultiPlan_201() throws Exception {
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    mvc.perform(
            post(uri)
                .content(TestUtil.toJson(createPaymentPositionV3(2, 2)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());
  }

  @Test
  void createDebtPosition_400() throws Exception {
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    PaymentPositionModelV3 paymentPositionModelV3 = createPaymentPositionV3(2, 2);
    paymentPositionModelV3.getPaymentOption().forEach(po -> po.getInstallments().forEach(inst -> inst.setIuv("sameIuv")));
    mvc.perform(
                    post(uri)
                            .content(TestUtil.toJson(paymentPositionModelV3))
                            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isBadRequest());
  }

  @Test
  void updateDebtPosition_200_1() throws Exception {
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    PaymentPositionModelV3 ppv3 = createPaymentPositionV3(2, 1);

    mvc.perform(post(uri).content(TestUtil.toJson(ppv3)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String positionUri =
        String.format("/v3/organizations/%s/debtpositions/%s", ORG_FISCAL_CODE, ppv3.getIupd());
    ppv3.setOfficeName("UpdatedOfficeName");
    mvc.perform(
            put(positionUri).content(TestUtil.toJson(ppv3)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

  @Test
  void deleteDebtPosition_200_1() throws Exception {
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    PaymentPositionModelV3 ppv3 = createPaymentPositionV3(2, 1);

    mvc.perform(post(uri).content(TestUtil.toJson(ppv3)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String positionUri =
        String.format("/v3/organizations/%s/debtpositions/%s", ORG_FISCAL_CODE, ppv3.getIupd());
    mvc.perform(delete(positionUri).content(TestUtil.toJson(ppv3))).andExpect(status().isOk());
  }

  @Test
  void publishDebtPosition_200_1() throws Exception {
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    PaymentPositionModelV3 ppv3 = createPaymentPositionV3(2, 1);

    mvc.perform(post(uri).content(TestUtil.toJson(ppv3)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    String positionUri =
        String.format(
            "/v3/organizations/%s/debtpositions/%s/publish", ORG_FISCAL_CODE, ppv3.getIupd());
    mvc.perform(post(positionUri).content(TestUtil.toJson(ppv3))).andExpect(status().isOk());
  }

  // ################### UTILS #################

  private PaymentPositionModelV3 createPaymentPositionV3(int numberOfPO, int numberOfInstallment) {
    PaymentPositionModelV3 paymentPosition = new PaymentPositionModelV3();
    paymentPosition.setIupd(String.format("IUPD-%s", RandomStringUtils.randomAlphanumeric(10)));
    paymentPosition.setCompanyName("CompanyName");

    for (int i = 0; i < numberOfPO; i++)
      paymentPosition.addPaymentOption(createPaymentOptionV3(numberOfInstallment));

    return paymentPosition;
  }

  private PaymentOptionModelV3 createPaymentOptionV3(int numberOfInstallment) {
    PaymentOptionModelV3 pov3 = new PaymentOptionModelV3();
    pov3.setSwitchToExpired(false);

    DebtorModel debtor = new DebtorModel();
    debtor.setType(Type.F);
    debtor.setFiscalCode("ABCDEF00A00A000A");
    debtor.setFullName("Full Name");
    pov3.setDebtor(debtor);

    for (int i = 0; i < numberOfInstallment; i++) pov3.addInstallment(createInstallment());

    return pov3;
  }

  private InstallmentModel createInstallment() {
    InstallmentModel inst = new InstallmentModel();
    inst.setIuv(RandomStringUtils.randomNumeric(17));
    inst.setAmount(100L);
    inst.setDescription("Description");
    inst.setDueDate(LocalDateTime.now().plusDays(60));
    ArrayList<InstallmentMetadataModel> instMetadataList =
        new ArrayList<>(
            Arrays.asList(
                new InstallmentMetadataModel("key1", "value1"),
                new InstallmentMetadataModel("key2", "value2")));
    inst.setInstallmentMetadata(instMetadataList);

    TransferModel transfer = getTransferModel();
    inst.setTransfer(List.of(transfer));

    return inst;
  }

  private static TransferModel getTransferModel() {
    TransferModel transfer = new TransferModel();
    transfer.setIdTransfer("1");
    transfer.setCompanyName("CompanyName");
    transfer.setIban("IT75I0306902887100000300015");
    transfer.setAmount(100L);
    transfer.setRemittanceInformation("remittance information");
    transfer.setCategory("10/22252/20");
    ArrayList<TransferMetadataModel> transferMetadataList =
        new ArrayList<>(
            Arrays.asList(
                new TransferMetadataModel("key1", "value1"),
                new TransferMetadataModel("key2", "value2")));
    transfer.setTransferMetadata(transferMetadataList);
    return transfer;
  }
}
