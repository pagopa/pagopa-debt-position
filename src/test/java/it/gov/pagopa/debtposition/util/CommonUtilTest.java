package it.gov.pagopa.debtposition.util;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class CommonUtilTest {

  @Test
  void getSegregationCodeEnd_shouldIncrementNumericCodePreservingLength() {
    assertEquals("02", CommonUtil.getSegregationCodeEnd("01"));
    assertEquals("10", CommonUtil.getSegregationCodeEnd("09"));
    assertEquals("19", CommonUtil.getSegregationCodeEnd("18"));
    assertEquals("20", CommonUtil.getSegregationCodeEnd("19"));
    assertEquals("99", CommonUtil.getSegregationCodeEnd("98"));
  }
}
