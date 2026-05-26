package it.gov.pagopa.debtposition.controller.pd.validator;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import it.gov.pagopa.debtposition.model.pd.PaymentOptionMetadataModel;
import it.gov.pagopa.debtposition.model.pd.TransferMetadataModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;

class UniqueMetadataKeysValidatorTest {

  private final UniqueMetadataKeysValidator validator = new UniqueMetadataKeysValidator();

  @Test
  void isValid_withNullList_returnsFalse() {
    assertFalse(validator.isValid(null, null));
  }

  @Test
  void isValid_withUniquePaymentOptionMetadataKeys_returnsTrue() {
    List<PaymentOptionMetadataModel> metadata =
        List.of(
            paymentOptionMetadata("key-1", "value-1"),
            paymentOptionMetadata("key-2", "value-2"));

    assertTrue(validator.isValid(metadata, null));
  }

  @Test
  void isValid_withDuplicatedPaymentOptionMetadataKeys_returnsFalse() {
    List<PaymentOptionMetadataModel> metadata =
        List.of(
            paymentOptionMetadata("same-key", "value-1"),
            paymentOptionMetadata("same-key", "value-2"));

    assertFalse(validator.isValid(metadata, null));
  }

  @Test
  void isValid_withUniqueTransferMetadataKeys_returnsTrue() {
    List<TransferMetadataModel> metadata =
        List.of(
            transferMetadata("transfer-key-1", "value-1"),
            transferMetadata("transfer-key-2", "value-2"));

    assertTrue(validator.isValid(metadata, null));
  }

  @Test
  void isValid_withDuplicatedTransferMetadataKeys_returnsFalse() {
    List<TransferMetadataModel> metadata =
        List.of(
            transferMetadata("same-transfer-key", "value-1"),
            transferMetadata("same-transfer-key", "value-2"));

    assertFalse(validator.isValid(metadata, null));
  }

  @Test
  void isValid_withNullItem_returnsFalse() {
    List<PaymentOptionMetadataModel> metadata =
        new java.util.ArrayList<>(List.of(paymentOptionMetadata("key-1", "value-1")));

    metadata.add(null);

    assertFalse(validator.isValid(metadata, null));
  }
  
  @Test
  void isValid_withEmptyList_returnsTrue() {
    assertTrue(validator.isValid(Collections.emptyList(), null));
  }

  @Test
  void isValid_withNullKey_ignoresKeyAndReturnsTrue() {
    List<MetadataWithNullKey> metadata = List.of(new MetadataWithNullKey(), new MetadataWithNullKey());

    assertTrue(validator.isValid(metadata, null));
  }

  @Test
  void isValid_withObjectWithoutGetKey_ignoresItemAndReturnsTrue() {
    List<ObjectWithoutGetKey> metadata = List.of(new ObjectWithoutGetKey(), new ObjectWithoutGetKey());

    assertTrue(validator.isValid(metadata, null));
  }

  @Test
  void isValid_withGetKeyReturningNonString_ignoresItemAndReturnsTrue() {
    List<MetadataWithNonStringKey> metadata =
        List.of(new MetadataWithNonStringKey(), new MetadataWithNonStringKey());

    assertTrue(validator.isValid(metadata, null));
  }
  
  @Test
  void isValid_withUniqueInstallmentMetadataKeys_returnsTrue() {
    List<InstallmentMetadataModel> metadata =
        List.of(
            new InstallmentMetadataModel("installment-key-1", "value-1"),
            new InstallmentMetadataModel("installment-key-2", "value-2"));

    assertTrue(validator.isValid(metadata, null));
  }

  @Test
  void isValid_withDuplicatedInstallmentMetadataKeys_returnsFalse() {
    List<InstallmentMetadataModel> metadata =
        List.of(
            new InstallmentMetadataModel("same-installment-key", "value-1"),
            new InstallmentMetadataModel("same-installment-key", "value-2"));

    assertFalse(validator.isValid(metadata, null));
  }

  private PaymentOptionMetadataModel paymentOptionMetadata(String key, String value) {
    PaymentOptionMetadataModel metadata = new PaymentOptionMetadataModel();
    metadata.setKey(key);
    metadata.setValue(value);
    return metadata;
  }

  private TransferMetadataModel transferMetadata(String key, String value) {
    TransferMetadataModel metadata = new TransferMetadataModel();
    metadata.setKey(key);
    metadata.setValue(value);
    return metadata;
  }

  private static class MetadataWithNullKey {
    public String getKey() {
      return null;
    }
  }

  private static class ObjectWithoutGetKey {
    public String getValue() {
      return "value";
    }
  }

  private static class MetadataWithNonStringKey {
    public Long getKey() {
      return 1L;
    }
  }
}