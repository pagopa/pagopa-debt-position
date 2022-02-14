package it.gov.pagopa.hubpa.functions;

import it.gov.pagopa.hubpa.models.OptionsMessage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@ExtendWith(MockitoExtension.class)
class OptionsMessageTest {

    @Test
    void optionMessageTest() {

        OptionsMessage optionsMessage = new OptionsMessage();
        optionsMessage.setIdFlow("idFlow");
        optionsMessage.setDateFlow("2015-04-24 11:15:00");

        optionsMessage.setIuvs(new String[]{"identificativoUnivocoVersamento1","identificativoUnivocoVersamento2"});

        assertNotNull(optionsMessage.getDateFlow());
        assertNotNull(optionsMessage.getIdFlow());
        assertEquals(2, optionsMessage.getIuvs().length);
    }

}