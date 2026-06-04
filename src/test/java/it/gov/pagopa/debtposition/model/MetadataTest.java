package it.gov.pagopa.debtposition.model;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

class MetadataTest {

  @Test
  void paymentOptionModel_setPaymentOptionMetadataWithNull_setsEmptyList() {
    PaymentOptionModel model = new PaymentOptionModel();

    model.setPaymentOptionMetadata(null);

    assertNotNull(model.getPaymentOptionMetadata());
    assertTrue(model.getPaymentOptionMetadata().isEmpty());
  }

  @Test
  void transferModel_setTransferMetadataWithNull_setsEmptyList() {
    TransferModel model = new TransferModel();

    model.setTransferMetadata(null);

    assertNotNull(model.getTransferMetadata());
    assertTrue(model.getTransferMetadata().isEmpty());
  }

  @Test
  void installmentModel_setInstallmentMetadataWithNull_setsEmptyList() {
    InstallmentModel model = new InstallmentModel();

    model.setInstallmentMetadata(null);

    assertNotNull(model.getInstallmentMetadata());
    assertTrue(model.getInstallmentMetadata().isEmpty());
  }
  
  @Test
  void paymentOptionModel_deserializeExplicitNullPaymentOptionMetadata_setsEmptyList()
      throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    PaymentOptionModel model =
        objectMapper.readValue(
            """
            {
              "paymentOptionMetadata": null
            }
            """,
            PaymentOptionModel.class);

    assertNotNull(model.getPaymentOptionMetadata());
    assertTrue(model.getPaymentOptionMetadata().isEmpty());
  }

  @Test
  void transferModel_deserializeExplicitNullTransferMetadata_setsEmptyList()
      throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    TransferModel model =
        objectMapper.readValue(
            """
            {
              "transferMetadata": null
            }
            """,
            TransferModel.class);

    assertNotNull(model.getTransferMetadata());
    assertTrue(model.getTransferMetadata().isEmpty());
  }

  @Test
  void installmentModel_deserializeExplicitNullInstallmentMetadata_setsEmptyList()
      throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    InstallmentModel model =
        objectMapper.readValue(
            """
            {
              "installmentMetadata": null
            }
            """,
            InstallmentModel.class);

    assertNotNull(model.getInstallmentMetadata());
    assertTrue(model.getInstallmentMetadata().isEmpty());
  }
}