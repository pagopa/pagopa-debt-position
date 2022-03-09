package it.gov.pagopa.reporting.functions;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.logging.Logger;

import com.microsoft.azure.functions.ExecutionContext;

import it.gov.pagopa.reporting.service.GPDService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;


@ExtendWith(MockitoExtension.class)
class OptionFunctionTest {

    @Mock
    ExecutionContext context;

    @Spy
    UpdateOption function;

    @Mock
    GPDService gpdServiceFake;

    @Test
    void runOkTest() {

        Logger logger = Logger.getLogger("InfoLogging");

    String message =
        "{\"idPA\":\"77777777777\",\"idFlow\":\"00595780131\",\"flowDate\":1627293600000,\"paymentOptions\":[{\"optionId\":\"09909090909\",\"transferId\":\"1\"},{\"optionId\":\"09909090909\",\"transferId\":\"1\"}]}";
        when(context.getLogger()).thenReturn(logger);

        when(function.getGPDServiceInstance()).thenReturn(gpdServiceFake);

        function.run(message, context);

        verify(context, times(1)).getLogger();

        verify(gpdServiceFake, times(2)).setReport(anyString(), any());
    }

    @Test
    void runWithInvalidMessageTest() {

        Logger logger = Logger.getLogger("InfoLogging");

        String message = "invalidMessage";
        when(context.getLogger()).thenReturn(logger);

        function.run(message, context);

        verify(context, times(1)).getLogger();
    }

    @Test
    void runExceptionTest() {

        Logger logger = Logger.getLogger("InfoLogging");

        String message = "{\"idFlow\":\"00595780131\",\"iuvs\":[\"1627293600000\"],\"dateFlow\":1627293600000}";
        when(context.getLogger()).thenReturn(logger);

        function.run(message, context);

        verify(context, times(1)).getLogger();
    }
}
