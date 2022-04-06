package it.gov.pagopa.reporting.service;

import it.gov.pagopa.reporting.models.PaymentOption;
import org.junit.jupiter.api.Test;

import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.*;

class GPDServiceTest {



    @Test
    void setReportError() {
        Logger logger = Logger.getLogger("testlogging");

        var gpdService = GPDService.getInstance();
        PaymentOption option = new PaymentOption();
        option.setOptionId("1");
        option.setTransferId(1);
        boolean result = gpdService.setReport("1", option, logger);
        assertFalse(result);
    }
}
