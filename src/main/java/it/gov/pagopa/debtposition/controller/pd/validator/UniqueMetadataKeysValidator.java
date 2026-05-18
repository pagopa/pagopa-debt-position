package it.gov.pagopa.debtposition.controller.pd.validator;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class UniqueMetadataKeysValidator implements ConstraintValidator<UniqueMetadataKeys, List<?>> {

  @Override
  public boolean isValid(List<?> metadata, ConstraintValidatorContext context) {
    if (metadata == null || metadata.isEmpty()) {
      return true;
    }

    Set<String> keys = new HashSet<>();

    for (Object item : metadata) {
      if (item == null) {
        continue;
      }

      String key = extractKey(item);

      if (key != null && !keys.add(key)) {
        return false;
      }
    }

    return true;
  }

  private String extractKey(Object item) {
    try {
      Method getter = item.getClass().getMethod("getKey");
      Object value = getter.invoke(item);
      return value instanceof String key ? key : null;
    } catch (ReflectiveOperationException e) {
      return null;
    }
  }
}