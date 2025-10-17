package it.gov.pagopa.debtposition.mapper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import java.util.Collections;
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
    assertEquals(1, entity.getPaymentOption().size());

    PaymentOption po = entity.getPaymentOption().get(0);
    assertEquals("ABCDEF12G34H567I", po.getDebtorFiscalCode());
    assertEquals("Mario Rossi", po.getDebtorFullName());
    assertEquals("Via Roma", po.getDebtorStreetName());
    assertEquals("10", po.getDebtorCivicNumber());
    assertEquals("00100", po.getDebtorPostalCode());
    assertEquals("Roma", po.getDebtorCity());
    assertEquals("RM", po.getDebtorProvince());
    assertEquals("Lazio", po.getDebtorRegion());
    assertEquals("IT", po.getDebtorCountry());
    assertEquals("mario.rossi@example.com", po.getDebtorEmail());
    assertEquals("1234567890", po.getDebtorPhone());
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
    assertEquals("Mario Rossi", entity.getPaymentOption().get(0).getDebtorFullName());
    assertEquals("Roma", entity.getPaymentOption().get(0).getDebtorCity());
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
    assertEquals(1, entity.getPaymentOption().size());

    PaymentOption po = entity.getPaymentOption().get(0);
    assertEquals("ABCDEF12G34H567I", po.getDebtorFiscalCode());
    assertEquals("Mario Rossi", po.getDebtorFullName());
    assertEquals("Via Roma", po.getDebtorStreetName());
    assertEquals("10", po.getDebtorCivicNumber());
    assertEquals("00100", po.getDebtorPostalCode());
    assertEquals("Roma", po.getDebtorCity());
    assertEquals("RM", po.getDebtorProvince());
    assertEquals("Lazio", po.getDebtorRegion());
    assertEquals("IT", po.getDebtorCountry());
    assertEquals("mario.rossi@example.com", po.getDebtorEmail());
    assertEquals("1234567890", po.getDebtorPhone());
    assertEquals(Type.F, po.getDebtorType());

    assertEquals(0, entity.getPaymentOption().get(0).getInstallment().get(0).getTransfer().size());
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
    assertEquals(1, entity.getPaymentOption().size());

    PaymentOption po = entity.getPaymentOption().get(0);
    assertEquals("ABCDEF12G34H567I", po.getDebtorFiscalCode());
    assertEquals("Mario Rossi", po.getDebtorFullName());
    assertEquals("Via Roma", po.getDebtorStreetName());
    assertEquals("10", po.getDebtorCivicNumber());
    assertEquals("00100", po.getDebtorPostalCode());
    assertEquals("Roma", po.getDebtorCity());
    assertEquals("RM", po.getDebtorProvince());
    assertEquals("Lazio", po.getDebtorRegion());
    assertEquals("IT", po.getDebtorCountry());
    assertEquals("mario.rossi@example.com", po.getDebtorEmail());
    assertEquals("1234567890", po.getDebtorPhone());
    assertEquals(Type.F, po.getDebtorType());

    assertEquals(1, entity.getPaymentOption().get(0).getInstallment().get(0).getTransfer().size());
  }
}
