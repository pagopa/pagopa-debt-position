package it.gov.pagopa.debtposition.controller;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItem;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.hamcrest.core.IsNull;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.env.Environment;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

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

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class DebtPositionControllerV3Test {
  @Autowired private MockMvc mvc;
  @Autowired private Environment env;

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
                .content(TestUtil.toJson(createPaymentPositionV3(1, 1)))
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
  void createDebtPosition_201_multiPlan() throws Exception {
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    mvc.perform(
            post(uri)
                .content(TestUtil.toJson(createPaymentPositionV3(2, 2)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());
  }
  

  @Test
  void createDebtPosition_400_duplicateIuvAcrossInstallments() throws Exception {
    final String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    final String DUPL_IUV = "10000000000009999";

    // same IUV
    PaymentPositionModelV3 pp = new PaymentPositionModelV3();
    pp.setIupd("IUPD-" + randomAlphaNum(10));
    pp.setCompanyName("CompanyName");

    PaymentOptionModelV3 po1 = basePaymentOption(false);
    po1.getInstallments().add(buildInstallmentWithIuv(DUPL_IUV, 500L, "Saldo unico", LocalDateTime.now().plusDays(30)));

    PaymentOptionModelV3 po2 = basePaymentOption(true);
    po2.getInstallments().add(buildInstallmentWithIuv(DUPL_IUV, 600L, "Piano A - Rata 1/1", LocalDateTime.now().plusDays(40)));

    pp.setPaymentOption(List.of(po1, po2));

    mvc.perform(
            post(uri)
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(jsonPath("$.title").value("BAD REQUEST"))
        .andExpect(jsonPath("$.status").value(400));
  }
  
  @Test
  void createDebtPosition_400_noInstallments() throws Exception {
    // given: 1 payment option and 0 installments
    String uri = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    PaymentPositionModelV3 ppNoInst = createPaymentPositionV3(1, 0); // <-- 0 installments

    mvc.perform(
            post(uri)
                .content(TestUtil.toJson(ppNoInst))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(jsonPath("$.detail").value(containsString("installments")));
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
  
  @Test
  void createDebtPosition_MixedValidity_toPublishTrue() throws Exception {
      String uri = String.format("/v3/organizations/%s/debtpositions?toPublish=true", ORG_FISCAL_CODE);
      PaymentPositionModelV3 paymentPosition = createPaymentPositionV3(2, 1);
      
      // PO_1 with validity valued
      paymentPosition.getPaymentOption().get(0).setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plusDays(1));
      // PO_2 with validity = null
      paymentPosition.getPaymentOption().get(1).setValidityDate(null);

      mvc.perform(
              post(uri)
                  .content(TestUtil.toJson(paymentPosition))
                  .contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isCreated())
          .andExpect(content().contentType(MediaType.APPLICATION_JSON))
          // then: PO_1 maintains validity, PO_2 is populated (currentDate)
          .andExpect(jsonPath("$.paymentOption[0].validityDate").isNotEmpty())
          .andExpect(jsonPath("$.paymentOption[1].validityDate").isNotEmpty());
  }
  
  @Test
  void createDebtPosition_MixedValidity_toPublishFalse() throws Exception {
      String uri = String.format("/v3/organizations/%s/debtpositions?toPublish=false", ORG_FISCAL_CODE);
      PaymentPositionModelV3 paymentPosition = createPaymentPositionV3(2, 1);
      
      // PO_1 with validity valued
      paymentPosition.getPaymentOption().get(0).setValidityDate(LocalDateTime.now(ZoneOffset.UTC).plusDays(1));
      // PO_2 with validity = null
      paymentPosition.getPaymentOption().get(1).setValidityDate(null);
      
      var mvcResult =
    	        mvc.perform(post(uri)
    	                .content(TestUtil.toJson(paymentPosition))
    	                .contentType(MediaType.APPLICATION_JSON))
    	           .andExpect(status().isCreated())
    	           .andExpect(content().contentType(MediaType.APPLICATION_JSON))
    	           .andReturn();
      
      String flag = env.getProperty("database.migration.fields.read.from", "");
      boolean readFromPP = "READ_FROM_PAYMENT_POSITION".equals(flag);
      
      // parse response
      ObjectMapper om = new ObjectMapper();
      JsonNode root = om.readTree(mvcResult.getResponse().getContentAsByteArray());
      JsonNode poArray = root.path("paymentOption");
      JsonNode po0 = poArray.get(0);
      JsonNode po1 = poArray.get(1);
      JsonNode vd0 = po0.get("validityDate");
      JsonNode vd1 = po1.get("validityDate");
      
      if (!readFromPP) {
    	  // correct behaviour: PO_1 remains valued, PO_2 remains null
    	  assertThat(vd0).isNotNull();
          assertThat(vd0.isNull()).isFalse();
          assertThat(vd1 == null || vd1.isNull()).isTrue();
      }
      else {
    	  // TODO remove when validityDate is read from payment option
    	  // fallback behaviour: both valued and equal to each other
    	  assertThat(vd0).isNotNull();
    	  assertThat(vd0.isNull()).isFalse();
    	  assertThat(vd1).isNotNull();
    	  assertThat(vd1.isNull()).isFalse();
    	  assertThat(vd0.asText()).isEqualTo(vd1.asText());
      }
  }

  // ################### UTILS #################

  public static PaymentPositionModelV3 createPaymentPositionV3(int numberOfPO, int numberOfInstallment) {
    PaymentPositionModelV3 paymentPosition = new PaymentPositionModelV3();
    paymentPosition.setIupd(String.format("IUPD-%s", randomAlphaNum(10)));
    paymentPosition.setCompanyName("CompanyName");

    for (int i = 0; i < numberOfPO; i++)
      paymentPosition.addPaymentOption(createPaymentOptionV3(numberOfInstallment));

    return paymentPosition;
  }

  private static PaymentOptionModelV3 createPaymentOptionV3(int numberOfInstallment) {
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

  private static InstallmentModel createInstallment() {
    InstallmentModel inst = new InstallmentModel();
    inst.setIuv(randomAlphaNum(17));
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
  
  private PaymentOptionModelV3 basePaymentOption(boolean switchToExpired) {
	  PaymentOptionModelV3 po = new PaymentOptionModelV3();
	  po.setSwitchToExpired(switchToExpired);
	  DebtorModel debtor = new DebtorModel();
	  debtor.setType(Type.F);
	  debtor.setFiscalCode("ABCDEF00A00A000A");
	  debtor.setFullName("Full Name");
	  debtor.setStreetName("Via Roma");
	  debtor.setCivicNumber("10");
	  debtor.setPostalCode("00100");
	  debtor.setCity("Roma");
	  debtor.setProvince("RM");
	  debtor.setRegion("Lazio");
	  debtor.setCountry("IT");
	  debtor.setEmail("full.name@example.com");
	  debtor.setPhone("+39061234567");
	  po.setDebtor(debtor);
	  po.setValidityDate(LocalDateTime.now().plusDays(1));
	  po.setRetentionDate(LocalDateTime.now().plusDays(90));
	  return po;
	}

	private InstallmentModel buildInstallmentWithIuv(String iuv, long amount, String description, LocalDateTime due) {
	  InstallmentModel inst = new InstallmentModel();
	  inst.setIuv(iuv);
	  inst.setAmount(amount);
	  inst.setDescription(description);
	  inst.setDueDate(due);

	  TransferModel tr = new TransferModel();
	  tr.setIdTransfer("1");
	  tr.setCompanyName("CompanyName");
	  tr.setIban("IT75I0306902887100000300015");
	  tr.setAmount(amount); // coerente con la rata
	  tr.setRemittanceInformation("remittance information");
	  tr.setCategory("10/22252/20");
	  tr.setOrganizationFiscalCode(ORG_FISCAL_CODE);

	  inst.setTransfer(List.of(tr));

	  ArrayList<InstallmentMetadataModel> instMetadataList = new ArrayList<>(
	      Arrays.asList(
	          new InstallmentMetadataModel("key1", "value1"),
	          new InstallmentMetadataModel("key2", "value2")));
	  inst.setInstallmentMetadata(instMetadataList);

	  return inst;
	}
	
	private static String randomAlphaNum(int len) {
		if (len <= 0)
			return "";

		String s = java.util.UUID.randomUUID().toString().replace("-", "");
		if (s.length() >= len) {
			return s.substring(0, len);
		}
		String formatSpec = String.format("%%-%ss", String.valueOf(len));
		return String.format(formatSpec, s).replace(' ', '0');
	}
}
