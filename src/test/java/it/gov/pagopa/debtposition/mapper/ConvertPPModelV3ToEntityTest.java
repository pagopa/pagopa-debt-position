package it.gov.pagopa.debtposition.mapper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.pd.TransferMetadataModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import java.util.Collections;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.modelmapper.spi.MappingContext;

class ConvertPPModelV3ToEntityTest {

  @Test
  void poNull() {
    // Arrange
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    model.setPaymentOption(null);

    ConvertPPModelV3ToEntity mapper = new ConvertPPModelV3ToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Comune", entity.getCompanyName());
  }

  @Test
  void poEmpty() {
    // Arrange
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    PaymentOptionModelV3 paymentOptionModelV3 = new PaymentOptionModelV3();
    paymentOptionModelV3.setDescription("rata");
    paymentOptionModelV3.setSwitchToExpired(true);

    model.setPaymentOption(Collections.singletonList(paymentOptionModelV3));

    ConvertPPModelV3ToEntity mapper = new ConvertPPModelV3ToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Comune", entity.getCompanyName());

    assertEquals(0, entity.getPaymentOption().size());
  }

  @Test
  void onePo() {
    // Arrange
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    PaymentOptionModelV3 paymentOptionModelV3 = new PaymentOptionModelV3();
    paymentOptionModelV3.setDescription("rata");
    paymentOptionModelV3.setSwitchToExpired(true);

    InstallmentModel installmentModel = new InstallmentModel();
    installmentModel.setAmount(1L);
    installmentModel.setIuv("123");
    paymentOptionModelV3.setInstallments(Collections.singletonList(installmentModel));

    model.setPaymentOption(Collections.singletonList(paymentOptionModelV3));

    ConvertPPModelV3ToEntity mapper = new ConvertPPModelV3ToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Comune", entity.getCompanyName());

    assertEquals(1, entity.getPaymentOption().size());
  }

  @Test
  void oneTransfer() {
    // Arrange
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    PaymentOptionModelV3 paymentOptionModelV3 = new PaymentOptionModelV3();
    paymentOptionModelV3.setDescription("rata");
    paymentOptionModelV3.setSwitchToExpired(true);

    InstallmentModel installmentModel = new InstallmentModel();
    installmentModel.setAmount(1L);
    installmentModel.setIuv("123");

    TransferModel transferModel = new TransferModel();
    transferModel.setIdTransfer("1");
    transferModel.setAmount(1L);

    installmentModel.setTransfer(Collections.singletonList(transferModel));

    paymentOptionModelV3.setInstallments(Collections.singletonList(installmentModel));

    model.setPaymentOption(Collections.singletonList(paymentOptionModelV3));

    ConvertPPModelV3ToEntity mapper = new ConvertPPModelV3ToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Comune", entity.getCompanyName());

    assertEquals(1, entity.getPaymentOption().size());
    assertEquals(1, entity.getPaymentOption().get(0).getInstallment().get(0).getTransfer().size());
    assertEquals("1", entity.getPaymentOption().get(0).getInstallment().get(0).getTransfer().get(0).getTransferId());
  }

  @Test
  void onePOMetadata() {
    // Arrange
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    PaymentOptionModelV3 paymentOptionModelV3 = new PaymentOptionModelV3();
    paymentOptionModelV3.setDescription("rata");
    paymentOptionModelV3.setSwitchToExpired(true);

    InstallmentModel installmentModel = new InstallmentModel();
    installmentModel.setAmount(1L);
    installmentModel.setIuv("123");
    InstallmentMetadataModel installmentMetadataModel = new InstallmentMetadataModel();
    installmentMetadataModel.setKey("k");
    installmentMetadataModel.setValue("v");

    installmentModel.setInstallmentMetadata(Collections.singletonList(installmentMetadataModel));

    TransferModel transferModel = new TransferModel();
    transferModel.setIdTransfer("1");
    transferModel.setAmount(1L);

    installmentModel.setTransfer(Collections.singletonList(transferModel));

    paymentOptionModelV3.setInstallments(Collections.singletonList(installmentModel));

    model.setPaymentOption(Collections.singletonList(paymentOptionModelV3));

    ConvertPPModelV3ToEntity mapper = new ConvertPPModelV3ToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Comune", entity.getCompanyName());

    assertEquals(1, entity.getPaymentOption().size());
    assertEquals(1, entity.getPaymentOption().get(0).getInstallment().get(0).getTransfer().size());
    assertEquals("1", entity.getPaymentOption().get(0).getInstallment().get(0).getTransfer().get(0).getTransferId());
    // assertEquals(1, entity.getPaymentOption().get(0).getInstallment().get(0).getPaymentOptionMetadata().size()); // TODO
  }

  @Test
  void completeModel() {
    // Arrange
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    PaymentOptionModelV3 paymentOptionModelV3 = new PaymentOptionModelV3();
    paymentOptionModelV3.setDescription("rata");
    paymentOptionModelV3.setSwitchToExpired(true);

    InstallmentModel installmentModel = new InstallmentModel();
    installmentModel.setAmount(1L);
    installmentModel.setIuv("123");
    InstallmentMetadataModel installmentMetadataModel = new InstallmentMetadataModel();
    installmentMetadataModel.setKey("k");
    installmentMetadataModel.setValue("v");

    installmentModel.setInstallmentMetadata(Collections.singletonList(installmentMetadataModel));

    TransferModel transferModel = new TransferModel();
    transferModel.setIdTransfer("1");
    transferModel.setAmount(1L);

    TransferMetadataModel transferMetadataModel = new TransferMetadataModel();
    transferMetadataModel.setKey("k");
    transferMetadataModel.setValue("v");

    transferModel.setTransferMetadata(Collections.singletonList(transferMetadataModel));

    installmentModel.setTransfer(Collections.singletonList(transferModel));

    paymentOptionModelV3.setInstallments(Collections.singletonList(installmentModel));

    model.setPaymentOption(Collections.singletonList(paymentOptionModelV3));

    ConvertPPModelV3ToEntity mapper = new ConvertPPModelV3ToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Comune", entity.getCompanyName());

    assertEquals(1, entity.getPaymentOption().size());
    assertEquals(1, entity.getPaymentOption().get(0).getInstallment().get(0).getTransfer().size());
    assertEquals("1", entity.getPaymentOption().get(0).getInstallment().get(0).getTransfer().get(0).getTransferId());
    // TODO assertEquals(1, entity.getPaymentOption().get(0).getInstallment().get(0).getPaymentOptionMetadata().size());
    // TODO assertEquals(1, entity.getPaymentOption().get(0).getInstallment().get(0).getTransfer().get(0).getTransferMetadata().size());
  }
}
