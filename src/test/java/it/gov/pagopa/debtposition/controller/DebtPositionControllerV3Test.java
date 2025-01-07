package it.gov.pagopa.debtposition.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import java.time.LocalDateTime;
import java.util.List;
import org.apache.commons.lang3.RandomStringUtils;
import org.hamcrest.core.IsNull;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
public class DebtPositionControllerV3Test {
  @Autowired private MockMvc mvc;

  @Mock private ModelMapper modelMapperMock;

  private static final String ORG_FISCAL_CODE = "7777777777";

  @BeforeEach
  void setUp() {}

  @Test
  void createDebtPositionUniquePO_201() throws Exception {
    String URI = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    mvc.perform(
            post(URI)
                .content(TestUtil.toJson(createPaymentPositionV3(1)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].installments[0].iuv").isNotEmpty())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].installments[0].nav").isNotEmpty())
        .andExpect(
            MockMvcResultMatchers.jsonPath(
                    "$.paymentOption[0].installments[0].transfer[0].companyName")
                .value("CompanyName"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].validityDate")
                .value(IsNull.nullValue()));
  }

  @Test
  void createDebtPositionPartialPO_201() throws Exception {
    String URI = String.format("/v3/organizations/%s/debtpositions", ORG_FISCAL_CODE);
    mvc.perform(
            post(URI)
                .content(TestUtil.toJson(createPaymentPositionV3(2)))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].installments[0].iuv").isNotEmpty())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].installments[0].nav").isNotEmpty())
        .andExpect(
            MockMvcResultMatchers.jsonPath(
                    "$.paymentOption[0].installments[0].transfer[0].companyName")
                .value("CompanyName"))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.paymentOption[0].validityDate")
                .value(IsNull.nullValue()));
  }

  private PaymentPositionModelV3 createPaymentPositionV3(int numberOfPO) {
    PaymentPositionModelV3 ppV3 = new PaymentPositionModelV3();
    ppV3.setIupd(String.format("IUPD-%s", RandomStringUtils.randomAlphanumeric(10)));
    ppV3.setCompanyName("CompanyName");

    for (int i = 0; i < numberOfPO; i++) ppV3.addPaymentOption(createPaymentOptionV3());

    return ppV3;
  }

  private PaymentOptionModelV3 createPaymentOptionV3() {
    PaymentOptionModelV3 pov3 = new PaymentOptionModelV3();
    pov3.setSwitchToExpired(false);

    DebtorModel debtor = new DebtorModel();
    debtor.setType(Type.F);
    debtor.setFiscalCode("ABCDEF00A00A000A");
    debtor.setFullName("Full Name");
    pov3.setDebtor(debtor);

    InstallmentModel inst = new InstallmentModel();
    inst.setIuv(RandomStringUtils.randomNumeric(17));
    inst.setAmount(100L);
    inst.setDescription("Description");
    inst.setDueDate(LocalDateTime.now().plusDays(60));

    TransferModel transfer = new TransferModel();
    transfer.setIdTransfer("1");
    transfer.setCompanyName("CompanyName");
    transfer.setIban("IT75I0306902887100000300015");
    transfer.setAmount(100L);
    transfer.setRemittanceInformation("remittance information");
    transfer.setCategory("10/22252/20");
    inst.setTransfer(List.of(transfer));
    pov3.setInstallments(List.of(inst));

    return pov3;
  }
}
