package it.gov.pagopa.reporting.service;

import it.gov.pagopa.reporting.models.PaymentOption;
import it.gov.pagopa.reporting.models.RetryStep;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

class GPDServiceTest {

    @Test
    void setReportError() {
        Logger logger = Logger.getLogger("testlogging");

        var gpdService = GPDService.getInstance();
        PaymentOption option = new PaymentOption();
        option.setOptionId("1");
        option.setTransferId(1);
        var result = gpdService.setReport("1", option, logger, "123");
        assertEquals(RetryStep.RETRY, result);
    }

    @Test
    void setReport4xx() {
        Logger logger = Logger.getLogger("testlogging");

        var gpdService = Mockito.spy(GPDService.getInstance());
        when(gpdService.callSetReport(anyString(), any(), anyString())).thenReturn(404);

        PaymentOption option = new PaymentOption();
        option.setOptionId("1");
        option.setTransferId(1);

        var result = gpdService.setReport("1", option, logger, "123");
        assertEquals(RetryStep.ERROR, result);
    }

    @Test
    void setReportOk() {
        Logger logger = Logger.getLogger("testlogging");

        var gpdService = Mockito.spy(GPDService.getInstance());
        when(gpdService.callSetReport(anyString(), any(), anyString())).thenReturn(200);

        PaymentOption option = new PaymentOption();
        option.setOptionId("1");
        option.setTransferId(1);

        var result = gpdService.setReport("1", option, logger, "123");
        assertEquals(RetryStep.DONE, result);
    }
}
