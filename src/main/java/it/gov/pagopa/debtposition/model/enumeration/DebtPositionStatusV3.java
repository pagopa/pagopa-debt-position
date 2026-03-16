package it.gov.pagopa.debtposition.model.enumeration;

public enum DebtPositionStatusV3 {
  DRAFT,
  PUBLISHED,
  VALID,
  UNPAYABLE,
  PARTIALLY_PAID,
  PAID;

  public static DebtPositionStatusV3 convert(DebtPositionStatus debtPositionStatusV1) {
    return switch (debtPositionStatusV1) {
      case EXPIRED, INVALID -> DebtPositionStatusV3.UNPAYABLE;
      case REPORTED -> DebtPositionStatusV3.PAID;
      default -> DebtPositionStatusV3.valueOf(debtPositionStatusV1.name());
    };
  }

}
