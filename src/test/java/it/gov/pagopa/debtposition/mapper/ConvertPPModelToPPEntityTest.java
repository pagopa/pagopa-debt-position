package it.gov.pagopa.debtposition.mapper;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.modelmapper.spi.MappingContext;

class ConvertPPModelToPPEntityTest {

  @Test
  void shouldMapDebtorFieldsIntoPaymentOption() {
    // Arrange
    PaymentPositionModel model = new PaymentPositionModel();
    model.setFiscalCode("ABCDEF12G34H567I");
    model.setFullName("Mario Rossi");
    model.setStreetName("Via Roma");
    model.setCivicNumber("10");
    model.setPostalCode("00100");
    model.setCity("Roma");
    model.setProvince("RM");
    model.setRegion("Lazio");
    model.setCountry("IT");
    model.setEmail("mario.rossi@example.com");
    model.setPhone("1234567890");
    model.setType(Type.F);

    PaymentOptionModel poModel = new PaymentOptionModel();
    poModel.setAmount(10L);
    poModel.setIuv("123456IUV");
    model.setPaymentOption(Collections.singletonList(poModel));

    ConvertPPModelToPPEntity mapper = new ConvertPPModelToPPEntity();

    MappingContext<PaymentPositionModel, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Mario Rossi", entity.getFullName());
    assertEquals("Roma", entity.getCity());
    assertEquals(1, entity.getPaymentOption().size());

    PaymentOption po = entity.getPaymentOption().get(0);
    assertEquals("ABCDEF12G34H567I", po.getFiscalCode());
    assertEquals("Mario Rossi", po.getFullName());
    assertEquals("Via Roma", po.getStreetName());
    assertEquals("10", po.getCivicNumber());
    assertEquals("00100", po.getPostalCode());
    assertEquals("Roma", po.getCity());
    assertEquals("RM", po.getProvince());
    assertEquals("Lazio", po.getRegion());
    assertEquals("IT", po.getCountry());
    assertEquals("mario.rossi@example.com", po.getEmail());
    assertEquals("1234567890", po.getPhone());
    assertEquals(Type.F, po.getDebtorType());
  }

  @Test
  void poEmptyList() {
    // Arrange
    PaymentPositionModel model = new PaymentPositionModel();
    model.setFiscalCode("ABCDEF12G34H567I");
    model.setFullName("Mario Rossi");
    model.setStreetName("Via Roma");
    model.setCivicNumber("10");
    model.setPostalCode("00100");
    model.setCity("Roma");
    model.setProvince("RM");
    model.setRegion("Lazio");
    model.setCountry("IT");
    model.setEmail("mario.rossi@example.com");
    model.setPhone("1234567890");
    model.setType(Type.F);

    model.setPaymentOption(null);

    ConvertPPModelToPPEntity mapper = new ConvertPPModelToPPEntity();

    MappingContext<PaymentPositionModel, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Mario Rossi", entity.getFullName());
    assertEquals("Roma", entity.getCity());
    assertEquals(0, entity.getPaymentOption().size());
  }

  @Test
  void transferEmptyList() {
    // Arrange
    PaymentPositionModel model = new PaymentPositionModel();
    model.setFiscalCode("ABCDEF12G34H567I");
    model.setFullName("Mario Rossi");
    model.setStreetName("Via Roma");
    model.setCivicNumber("10");
    model.setPostalCode("00100");
    model.setCity("Roma");
    model.setProvince("RM");
    model.setRegion("Lazio");
    model.setCountry("IT");
    model.setEmail("mario.rossi@example.com");
    model.setPhone("1234567890");
    model.setType(Type.F);

    PaymentOptionModel poModel = new PaymentOptionModel();
    poModel.setAmount(10L);
    poModel.setIuv("123456IUV");
    poModel.setTransfer(null);
    poModel.setPaymentOptionMetadata(null);
    model.setPaymentOption(Collections.singletonList(poModel));

    ConvertPPModelToPPEntity mapper = new ConvertPPModelToPPEntity();

    MappingContext<PaymentPositionModel, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Mario Rossi", entity.getFullName());
    assertEquals("Roma", entity.getCity());
    assertEquals(1, entity.getPaymentOption().size());

    PaymentOption po = entity.getPaymentOption().get(0);
    assertEquals("ABCDEF12G34H567I", po.getFiscalCode());
    assertEquals("Mario Rossi", po.getFullName());
    assertEquals("Via Roma", po.getStreetName());
    assertEquals("10", po.getCivicNumber());
    assertEquals("00100", po.getPostalCode());
    assertEquals("Roma", po.getCity());
    assertEquals("RM", po.getProvince());
    assertEquals("Lazio", po.getRegion());
    assertEquals("IT", po.getCountry());
    assertEquals("mario.rossi@example.com", po.getEmail());
    assertEquals("1234567890", po.getPhone());
    assertEquals(Type.F, po.getDebtorType());

    assertEquals(0, entity.getPaymentOption().get(0).getTransfer().size());
  }

  @Test
  void poMetadataEmptyList() {
    // Arrange
    PaymentPositionModel model = new PaymentPositionModel();
    model.setFiscalCode("ABCDEF12G34H567I");
    model.setFullName("Mario Rossi");
    model.setStreetName("Via Roma");
    model.setCivicNumber("10");
    model.setPostalCode("00100");
    model.setCity("Roma");
    model.setProvince("RM");
    model.setRegion("Lazio");
    model.setCountry("IT");
    model.setEmail("mario.rossi@example.com");
    model.setPhone("1234567890");
    model.setType(Type.F);

    PaymentOptionModel poModel = new PaymentOptionModel();
    poModel.setAmount(10L);
    poModel.setIuv("123456IUV");

    TransferModel transferModel = new TransferModel();
    transferModel.setIdTransfer("1");
    transferModel.setTransferMetadata(null);
    transferModel.setAmount(1L);
    poModel.setTransfer(Collections.singletonList(transferModel));

    poModel.setPaymentOptionMetadata(null);
    model.setPaymentOption(Collections.singletonList(poModel));

    ConvertPPModelToPPEntity mapper = new ConvertPPModelToPPEntity();

    MappingContext<PaymentPositionModel, PaymentPosition> context =
        Mockito.mock(MappingContext.class);
    Mockito.when(context.getSource()).thenReturn(model);
    Mockito.when(context.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(context);

    assertNotNull(entity);
    assertEquals("Mario Rossi", entity.getFullName());
    assertEquals("Roma", entity.getCity());
    assertEquals(1, entity.getPaymentOption().size());

    PaymentOption po = entity.getPaymentOption().get(0);
    assertEquals("ABCDEF12G34H567I", po.getFiscalCode());
    assertEquals("Mario Rossi", po.getFullName());
    assertEquals("Via Roma", po.getStreetName());
    assertEquals("10", po.getCivicNumber());
    assertEquals("00100", po.getPostalCode());
    assertEquals("Roma", po.getCity());
    assertEquals("RM", po.getProvince());
    assertEquals("Lazio", po.getRegion());
    assertEquals("IT", po.getCountry());
    assertEquals("mario.rossi@example.com", po.getEmail());
    assertEquals("1234567890", po.getPhone());
    assertEquals(Type.F, po.getDebtorType());

    assertEquals(1, entity.getPaymentOption().get(0).getTransfer().size());
  }
  
  @Test
  void paymentPlanId_singleIsNull_andPlanGetsSharedUuid() {
    // Arrange: 1 single option and 2 installment options
    PaymentPositionModel model = baseDebtorV1();

    // Single (isPartialPayment = false)
    PaymentOptionModel single = new PaymentOptionModel();
    single.setIuv("S1");
    single.setAmount(10L);
    single.setIsPartialPayment(false);

    // Multi (isPartialPayment = true)
    PaymentOptionModel planA1 = new PaymentOptionModel();
    planA1.setIuv("A1");
    planA1.setAmount(10L);
    planA1.setIsPartialPayment(true);

    PaymentOptionModel planA2 = new PaymentOptionModel();
    planA2.setIuv("A2");
    planA2.setAmount(10L);
    planA2.setIsPartialPayment(true);

    model.setPaymentOption(List.of(single, planA1, planA2));

    ConvertPPModelToPPEntity mapper = new ConvertPPModelToPPEntity();
    MappingContext<PaymentPositionModel, PaymentPosition> ctx = Mockito.mock(MappingContext.class);
    Mockito.when(ctx.getSource()).thenReturn(model);
    Mockito.when(ctx.getDestination()).thenReturn(null);

    PaymentPosition entity = mapper.convert(ctx);

    // Assert: Single --> paymentPlanId nullo
    assertNull(findByIuv(entity, "S1").getPaymentPlanId());

    // installments → same shared UUID
    String pidA1 = findByIuv(entity, "A1").getPaymentPlanId();
    String pidA2 = findByIuv(entity, "A2").getPaymentPlanId();
    assertNotNull(pidA1);
    assertEquals(pidA1, pidA2);
    assertDoesNotThrow(() -> UUID.fromString(pidA1));
  }

  // ======== Helpers ========

  private static PaymentPositionModel baseDebtorV1() {
    PaymentPositionModel model = new PaymentPositionModel();
    model.setIupd("IUPD-TEST");
    model.setFiscalCode("ABCDEF12G34H567I");
    model.setFullName("Mario Rossi");
    model.setStreetName("Via Roma");
    model.setCivicNumber("10");
    model.setPostalCode("00100");
    model.setCity("Roma");
    model.setProvince("RM");
    model.setRegion("Lazio");
    model.setCountry("IT");
    model.setEmail("mario.rossi@example.com");
    model.setPhone("1234567890");
    model.setType(Type.F);
    return model;
  }
  
  private static PaymentOption findByIuv(PaymentPosition pp, String iuv) {
	  return pp.getPaymentOption().stream()
			  .filter(po -> iuv.equals(po.getIuv()))
			  .findFirst()
			  .orElseThrow(() -> new AssertionError("PaymentOption con iuv " + iuv + " non trovata"));
  }

}
