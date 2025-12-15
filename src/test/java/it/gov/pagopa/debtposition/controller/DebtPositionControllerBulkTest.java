package it.gov.pagopa.debtposition.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.client.NodeClient;
import it.gov.pagopa.debtposition.dto.MultipleIUPDDTO;
import it.gov.pagopa.debtposition.dto.MultiplePaymentPositionDTO;
import it.gov.pagopa.debtposition.mock.DebtPositionMock;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
class DebtPositionControllerBulkTest {
  @Autowired private MockMvc mvc;

  @Mock private ModelMapper modelMapperMock;

  @MockitoBean private NodeClient nodeClient;

  @Value("${nav.aux.digit}")
  private String auxDigit;

  @BeforeEach
  void setUp() {}

  /** CREATE MULTIPLE DEBT POSITIONS */
  @Test
  void createMultipleDebtPositions_201() throws Exception {
    mvc.perform(
            post("/organizations/12345678901_multiple/debtpositions/bulk")
                .content(TestUtil.toJson(DebtPositionMock.getMultipleDebtPositions_Mock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());
  }

  @Test
  void createMultipleDebtPositions_type_ACA_201() throws Exception {
    mvc.perform(
            post("/organizations/12345678901_multiple/debtpositions/bulk?serviceType=ACA")
                .content(TestUtil.toJson(DebtPositionMock.getMultipleDebtPositions_Mock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());
  }

  @Test
  void createMultipleDebtPositions_400() throws Exception {
    mvc.perform(
            post("/organizations/12345678901_multiple/debtpositions/bulk")
                .content(TestUtil.toJson(DebtPositionMock.getMultipleDebtPositions_400_Mock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void createMultipleDebtPositions_409() throws Exception {
    MultiplePaymentPositionDTO multiplePaymentPositionDTO =
        DebtPositionMock.getMultipleDebtPositions_Mock1();
    mvc.perform(
            post("/organizations/12345678901_multiple_409/debtpositions/bulk")
                .content(TestUtil.toJson(multiplePaymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    mvc.perform(
            post("/organizations/12345678901_multiple_409/debtpositions/bulk")
                .content(TestUtil.toJson(multiplePaymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isConflict());
  }

  /** UPDATE MULTIPLE DEBT POSITIONS */
  @Test
  void updateMultipleDebtPositions_200() throws Exception {
    MultiplePaymentPositionDTO multiplePaymentPositionDTO =
        DebtPositionMock.getMultipleDebtPositions_Mock2();
    mvc.perform(
            post("/organizations/12345678901_multiple/debtpositions/bulk")
                .content(TestUtil.toJson(multiplePaymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());
    mvc.perform(
            put("/organizations/12345678901_multiple/debtpositions")
                .content(TestUtil.toJson(multiplePaymentPositionDTO))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

    @Test
    void updateMultipleDebtPositions_404_differentOrgs() throws Exception {
        MultiplePaymentPositionDTO multiplePaymentPositionDTO =
                DebtPositionMock.getMultipleDebtPositions_Mock2();
        mvc.perform(
                        post("/organizations/77777777777/debtpositions/bulk")
                                .content(TestUtil.toJson(multiplePaymentPositionDTO))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isCreated());
        mvc.perform(
                        put("/organizations/12345678901_multiple/debtpositions")
                                .content(TestUtil.toJson(multiplePaymentPositionDTO))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }


    @Test
  void updateMultipleDebtPositions_400() throws Exception {
    mvc.perform(
            put("/organizations/12345678901_multiple/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMultipleDebtPositions_400_Mock2()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  @Test
  void updateMultipleDebtPositions_404() throws Exception {
    mvc.perform(
            put("/organizations/12345678901_multiple/debtpositions")
                .content(TestUtil.toJson(DebtPositionMock.getMultipleDebtPositions_Mock1()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON));
  }

  /** DELETE MULTIPLE DEBT POSITIONS */
  @Test
  void deleteMultipleDebtPositions_200() throws Exception {
    List<String> iupdList = new ArrayList<>();
    MultiplePaymentPositionDTO mpp = DebtPositionMock.getMultipleDebtPositions_Mock2();
    mpp.getPaymentPositions().forEach(pp -> iupdList.add(pp.getIupd()));
    // create if not exist
    mvc.perform(
        post("/organizations/12345678910/debtpositions/bulk")
            .content(TestUtil.toJson(mpp))
            .contentType(MediaType.APPLICATION_JSON));
    // delete IUPD list
    mvc.perform(
            delete("/organizations/12345678910/debtpositions")
                .content(
                    TestUtil.toJson(
                        MultipleIUPDDTO.builder().paymentPositionIUPDs(iupdList).build()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

  @Test
  void deleteMultipleDebtPositions_404() throws Exception {
    List<String> iupdList = new ArrayList<>();
    iupdList.add("ThisIUPDdoesntExist");
    mvc.perform(
            delete("/organizations/12345678901/debtpositions")
                .content(
                    TestUtil.toJson(
                        MultipleIUPDDTO.builder().paymentPositionIUPDs(iupdList).build()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isNotFound());
  }

    @Test
    void deleteMultipleDebtPositions_404_differentOrgs() throws Exception {
        List<String> iupdList = new ArrayList<>();
        MultiplePaymentPositionDTO mpp = DebtPositionMock.getMultipleDebtPositions_Mock2();
        mpp.getPaymentPositions().forEach(pp -> iupdList.add(pp.getIupd()));
        // create if not exist
        mvc.perform(
                post("/organizations/77777777777/debtpositions/bulk")
                        .content(TestUtil.toJson(mpp))
                        .contentType(MediaType.APPLICATION_JSON));
        // delete IUPD list
        mvc.perform(
                        delete("/organizations/12345678910/debtpositions")
                                .content(
                                        TestUtil.toJson(
                                                MultipleIUPDDTO.builder().paymentPositionIUPDs(iupdList).build()))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

  @Test
  void deleteMultipleDebtPositions_400() throws Exception {
    List<String> iupdList = new ArrayList<>();
    // create max +1 IUPDs
    for (int i = 0; i < 101; i++) iupdList.add("IUPD" + i);

    mvc.perform(
            delete("/organizations/12345678901_multiple/debtpositions")
                .content(
                    TestUtil.toJson(
                        MultipleIUPDDTO.builder().paymentPositionIUPDs(iupdList).build()))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isBadRequest());
  }
}
