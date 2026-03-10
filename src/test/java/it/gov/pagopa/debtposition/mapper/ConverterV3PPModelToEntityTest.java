package it.gov.pagopa.debtposition.mapper;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.pd.TransferMetadataModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import java.util.Collections;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.modelmapper.spi.MappingContext;

class ConverterV3PPModelToEntityTest {

  @Test
  void poNull() {
    // Arrange
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    model.setPaymentOption(null);

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();

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

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();

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

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();

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

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Comune", entity.getCompanyName());

    assertEquals(1, entity.getPaymentOption().size());
    assertEquals(1, entity.getPaymentOption().get(0).getTransfer().size());
    assertEquals("1", entity.getPaymentOption().get(0).getTransfer().get(0).getIdTransfer());
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

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Comune", entity.getCompanyName());

    assertEquals(1, entity.getPaymentOption().size());
    assertEquals(1, entity.getPaymentOption().get(0).getTransfer().size());
    assertEquals("1", entity.getPaymentOption().get(0).getTransfer().get(0).getIdTransfer());
    assertEquals(1, entity.getPaymentOption().get(0).getPaymentOptionMetadata().size());
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

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Comune", entity.getCompanyName());

    assertEquals(1, entity.getPaymentOption().size());
    assertEquals(1, entity.getPaymentOption().get(0).getTransfer().size());
    assertEquals("1", entity.getPaymentOption().get(0).getTransfer().get(0).getIdTransfer());
    assertEquals(1, entity.getPaymentOption().get(0).getPaymentOptionMetadata().size());
    assertEquals(
        1, entity.getPaymentOption().get(0).getTransfer().get(0).getTransferMetadata().size());
  }
  
  @Test
  void paymentPlanId_singleVsPlan() {
    // Arrange: 1 single option + 1 plan with 2 installments
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    // single option
    PaymentOptionModelV3 single = new PaymentOptionModelV3();
    single.setSwitchToExpired(true);
    InstallmentModel s1 = new InstallmentModel();
    s1.setIuv("S1");
    s1.setAmount(1L);
    single.setInstallments(Collections.singletonList(s1));

    // installment plan
    PaymentOptionModelV3 plan = new PaymentOptionModelV3();
    plan.setSwitchToExpired(true);
    InstallmentModel p1 = new InstallmentModel();
    p1.setIuv("P1A");
    p1.setAmount(1L);
    InstallmentModel p2 = new InstallmentModel();
    p2.setIuv("P1B");
    p2.setAmount(1L);
    plan.setInstallments(java.util.List.of(p1, p2));

    model.setPaymentOption(java.util.List.of(single, plan));

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();
    MappingContext<PaymentPositionModelV3, PaymentPosition> ctx = Mockito.mock(MappingContext.class);
    Mockito.when(ctx.getSource()).thenReturn(model);
    Mockito.when(ctx.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(ctx);

    assertNotNull(entity);
    // single --> paymentPlanId null
    assertEquals(PaymentOption.SINGLE_OPTION, findByIuv(entity, "S1").getPaymentPlanId());

    // plan → same UUID for both installments 
    String pidA = findByIuv(entity, "P1A").getPaymentPlanId();
    String pidB = findByIuv(entity, "P1B").getPaymentPlanId();
    assertNotNull(pidA);
    assertEquals(pidA, pidB);
    // valid UUID
    assertDoesNotThrow(() -> UUID.fromString(pidA));
  }

  @Test
  void paymentPlanId_twoDistinctPlans_haveDifferentUuids() {
    // Arrange: two separate installment plans (2 installments each)
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    PaymentOptionModelV3 planA = new PaymentOptionModelV3();
    planA.setSwitchToExpired(true);
    InstallmentModel a1 = new InstallmentModel(); a1.setIuv("A1"); a1.setAmount(1L);
    InstallmentModel a2 = new InstallmentModel(); a2.setIuv("A2"); a2.setAmount(1L);
    planA.setInstallments(java.util.List.of(a1, a2));

    PaymentOptionModelV3 planB = new PaymentOptionModelV3();
    planB.setSwitchToExpired(true);
    InstallmentModel b1 = new InstallmentModel(); b1.setIuv("B1"); b1.setAmount(1L);
    InstallmentModel b2 = new InstallmentModel(); b2.setIuv("B2"); b2.setAmount(1L);
    planB.setInstallments(java.util.List.of(b1, b2));

    model.setPaymentOption(java.util.List.of(planA, planB));

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();
    MappingContext<PaymentPositionModelV3, PaymentPosition> ctx = Mockito.mock(MappingContext.class);
    Mockito.when(ctx.getSource()).thenReturn(model);
    Mockito.when(ctx.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(ctx);

    // Assert: different plans → different UUIDs
    String pidA = findByIuv(entity, "A1").getPaymentPlanId();
    String pidA2 = findByIuv(entity, "A2").getPaymentPlanId();
    String pidB = findByIuv(entity, "B1").getPaymentPlanId();
    String pidB2 = findByIuv(entity, "B2").getPaymentPlanId();

    assertNotNull(pidA); assertNotNull(pidB);
    assertEquals(pidA, pidA2);
    assertEquals(pidB, pidB2);
    assertNotEquals(pidA, pidB);
  }

  @Test
  void paymentPlanId_update_reusesExistingUuid() {
    PaymentPosition existing = new PaymentPosition();
    PaymentOption already = PaymentOption.builder().build();
    already.setIuv("X1");  // existing installment          
    already.setPaymentPlanId(UUID.randomUUID().toString()); // UUID already present
    existing.getPaymentOption().add(already);

    // source: contains X1 + new X2
    PaymentPositionModelV3 model = new PaymentPositionModelV3();
    model.setCompanyName("Comune");

    PaymentOptionModelV3 plan = new PaymentOptionModelV3();
    plan.setSwitchToExpired(true);
    InstallmentModel x1 = new InstallmentModel(); x1.setIuv("X1"); x1.setAmount(1L); // existing
    InstallmentModel x2 = new InstallmentModel(); x2.setIuv("X2"); x2.setAmount(1L); // new
    plan.setInstallments(java.util.List.of(x1, x2));
    model.setPaymentOption(java.util.List.of(plan));

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();
    MappingContext<PaymentPositionModelV3, PaymentPosition> ctx = Mockito.mock(MappingContext.class);
    Mockito.when(ctx.getSource()).thenReturn(model);
    Mockito.when(ctx.getDestination()).thenReturn(existing); // <- simulate UPDATE

    PaymentPosition entity = mapper.convert(ctx);

    // Assert: the paymentPlanId of X2 must reuse the one already present on X1
    String pidX1 = findByIuv(entity, "X1").getPaymentPlanId();
    String pidX2 = findByIuv(entity, "X2").getPaymentPlanId();
    assertNotNull(pidX1);
    assertEquals(pidX1, pidX2);
  }
  
  @Test
  void shouldClearExistingStampFieldsWhenSourceStampIsNull() {
    PaymentPosition destination = new PaymentPosition();
    destination.setPaymentOption(new java.util.ArrayList<>());

    PaymentOption destinationPo = new PaymentOption();
    destinationPo.setIuv("IUV-V3-1");
    destinationPo.setPaymentPosition(destination);
    destinationPo.setTransfer(new java.util.ArrayList<>());
    destination.getPaymentOption().add(destinationPo);

    it.gov.pagopa.debtposition.entity.Transfer destinationTransfer =
        it.gov.pagopa.debtposition.entity.Transfer.builder()
            .idTransfer("1")
            .hashDocument("old-hash-v3")
            .stampType("01")
            .provincialResidence("RM")
            .paymentOption(destinationPo)
            .build();

    destinationPo.getTransfer().add(destinationTransfer);

    PaymentPositionModelV3 source = new PaymentPositionModelV3();
    source.setIupd("IUPD-V3-1");
    source.setCompanyName("Comune");

    PaymentOptionModelV3 po = new PaymentOptionModelV3();
    po.setSwitchToExpired(false);

    InstallmentModel installment = new InstallmentModel();
    installment.setIuv("IUV-V3-1");
    installment.setAmount(100L);

    TransferModel transfer = new TransferModel();
    transfer.setIdTransfer("1");
    transfer.setAmount(100L);
    transfer.setIban("IT58C0200805403000102985524");
    transfer.setStamp(null);

    installment.setTransfer(java.util.List.of(transfer));
    po.setInstallments(java.util.List.of(installment));
    source.setPaymentOption(java.util.List.of(po));

    ConverterV3PPModelToEntity mapper = new ConverterV3PPModelToEntity();

    MappingContext<PaymentPositionModelV3, PaymentPosition> context = Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(source);
    Mockito.when(context.getDestination()).thenReturn(destination);

    PaymentPosition result = mapper.convert(context);

    it.gov.pagopa.debtposition.entity.Transfer updatedTransfer =
        result.getPaymentOption().get(0).getTransfer().get(0);

    assertEquals("IT58C0200805403000102985524", updatedTransfer.getIban());
    assertNull(updatedTransfer.getHashDocument());
    assertNull(updatedTransfer.getStampType());
    assertNull(updatedTransfer.getProvincialResidence());
  }
  
//--- UTILITY: Find PaymentOption for IUV in the mapped entity ---
 private static PaymentOption findByIuv(PaymentPosition pp, String iuv) {
   return pp.getPaymentOption()
       .stream()
       .filter(po -> iuv.equals(po.getIuv()))
       .findFirst()
       .orElseThrow(() -> new AssertionError("PaymentOption with iuv " + iuv + " not found"));
 }
}
