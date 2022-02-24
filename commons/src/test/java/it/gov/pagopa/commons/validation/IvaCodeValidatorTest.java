package it.gov.pagopa.commons.validation;

import org.hibernate.validator.internal.util.annotation.AnnotationDescriptor;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import it.gov.pagopa.commons.annotation.validation.IvaCode;
import it.gov.pagopa.commons.annotation.validation.implement.IvaCodeValidator;

import static org.assertj.core.api.Assertions.assertThat;

class IvaCodeValidatorTest {

  @InjectMocks
  private IvaCodeValidator ivaCodeValid;

  @Test
  void realIvaShouldBeValid() {
    final IvaCodeValidator myIvaValidator = new IvaCodeValidator();

    final IvaCode myConstraint = new AnnotationDescriptor.Builder<>(IvaCode.class).build().getAnnotation();

    myIvaValidator.initialize(myConstraint);

    assertThat(myIvaValidator.isValid("15376371009", null)).isTrue();
  }

  @Test
  void fakeIvaShouldNotBeValid() {
    final IvaCodeValidator myIvaValidator = new IvaCodeValidator();

    final IvaCode myConstraint = new AnnotationDescriptor.Builder<>(IvaCode.class).build().getAnnotation();

    myIvaValidator.initialize(myConstraint);

    assertThat(myIvaValidator.isValid("15376371000", null)).isFalse();
  }

}
