package it.gov.pagopa.hubpa.payments.validator;

import static org.assertj.core.api.Assertions.assertThat;

import javax.validation.ConstraintValidatorContext;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import it.gov.pagopa.hubpa.payments.annotation.validation.implement.IbanCustomValidator;

@ExtendWith(MockitoExtension.class)
class IbanCustomValidatorTest {

    @InjectMocks
    private IbanCustomValidator ibanValidator;

    @Mock
    private ConstraintValidatorContext arg1;

    @Test
    void isValidBankIbanTest() {

        String validIban = "IT67P0300203280575369338247";

        assertThat(ibanValidator.isValid(validIban, arg1)).isTrue();
    }

    @Test
    void isInvalidIbanTest() {

        String validIban = "IT76N0300203280879483594666";

        assertThat(ibanValidator.isValid(validIban, arg1)).isFalse();
    }

    @Test
    void isValidPostalIbanTest() {

        String validIban = "IT60X0542811101000000123456";

        assertThat(ibanValidator.isValid(validIban, arg1)).isTrue();
    }
}
