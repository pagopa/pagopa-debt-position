package it.gov.pagopa.commons.validation;

import org.hibernate.validator.internal.util.annotation.AnnotationDescriptor;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import it.gov.pagopa.commons.annotation.validation.FiscalCode;
import it.gov.pagopa.commons.annotation.validation.implement.FiscalCodeValidator;

import static org.assertj.core.api.Assertions.assertThat;

class FiscalCodeValidatorTest {

  @InjectMocks
  private FiscalCodeValidator fiscalCodeValid;

  @Test
  void realCFShouldBeValid() {
    final FiscalCodeValidator myCfValidator = new FiscalCodeValidator();

    final FiscalCode myConstraint = new AnnotationDescriptor.Builder<>(FiscalCode.class).build().getAnnotation();

    myCfValidator.initialize(myConstraint);

    assertThat(myCfValidator.isValid("MNCGNN81M28E335F", null)).isTrue();
  }

  @Test
  void fakeCFShouldNotBeValid() {
    final FiscalCodeValidator myCfValidator = new FiscalCodeValidator();

    final FiscalCode myConstraint = new AnnotationDescriptor.Builder<>(FiscalCode.class).build().getAnnotation();

    myCfValidator.initialize(myConstraint);

    assertThat(myCfValidator.isValid("MNCGNN81M28E335Z", null)).isFalse();
  }

}
