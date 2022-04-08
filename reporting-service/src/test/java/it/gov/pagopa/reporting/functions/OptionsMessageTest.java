package it.gov.pagopa.reporting.functions;

import it.gov.pagopa.reporting.models.OptionsMessage;
import it.gov.pagopa.reporting.models.PaymentOption;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@ExtendWith(MockitoExtension.class)
class OptionsMessageTest {

    @Test
    void optionMessageTest() {

        OptionsMessage optionsMessage = new OptionsMessage();
        optionsMessage.setIdFlow("idFlow");
        optionsMessage.setFlowDate("2015-04-24 11:15:00");

        PaymentOption p1 = new PaymentOption("op1", 1, "NONE");
        PaymentOption p2 = new PaymentOption("op2", 2, "NONE");
        PaymentOption p3 = new PaymentOption("op3", 3, "NONE");

        optionsMessage.setPaymentOptions (List.of(p1,p2,p3));

        assertNotNull(optionsMessage.getFlowDate());
        assertNotNull(optionsMessage.getIdFlow());
        assertEquals(3, optionsMessage.getPaymentOptions().size());
    }

}
